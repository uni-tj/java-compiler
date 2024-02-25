{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}
{-# HLINT ignore "Redundant <$>" #-}
module SemanticCheck.Typecheck(typecheck) where

import           Control.Composition  ((.*))
import           Control.Lens         (Lens', (%%~), (%~), (.=))
import           Control.Monad.Except (ExceptT, MonadError (throwError), forM,
                                       join, runExceptT, void, zipWithM)
import           Control.Monad.Extra  (filterM, findM, forM_, ifM, when,
                                       whenJust, (&&^), (||^))
import           Control.Monad.State  (State, evalState, gets)
import           Data.Bool            (bool)
import           Data.Foldable        (foldlM)
import           Data.Function        ((&))
import           Data.Functor         ((<&>))
import           Data.Functor.Syntax  ((<$$>))
import           Data.List            (find, intercalate, nub, tails, uncons)
import           Data.List.NonEmpty   (unzip)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust, fromMaybe, isJust, isNothing,
                                       listToMaybe, mapMaybe)
import           Data.Tuple.Extra     (swap)
import           Debug.Trace          (traceShow)
import           Prelude              hiding (EQ, GT, LT, unzip)
import qualified SemanticCheck.StdLib as StdLib
import           SemanticCheck.Util
import qualified Types.AST            as AST
import           Types.Core           (AccessModifier (..), BinOperator (..),
                                       Identifier, Type (..), UnOparator (..))
import qualified Types.TAST           as TAST

{- General helper functions -}
hasNoDuplicates :: Eq a => [a] -> Bool
hasNoDuplicates xs = length xs == length (nub xs)

-- Lazy monadic operator choosing the first Just
(<<>^) :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
m1 <<>^ m2 = do
  may1 <- m1
  if isJust may1 then return may1 else m2

infixr 2 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f1 f2 x = f1 x || f2 x

condM :: Monad m => [(m Bool, m a)] -> m a
condM []          = error "Non-exhaustive patterns in condM"
condM ((p,v):pvs) = ifM p v (condM pvs)

otherwiseM :: Monad m => m Bool
otherwiseM = return otherwise

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing Nothing  action = action
whenNothing (Just _) _      = return ()

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip (bool id)
applyWhen' :: (a -> Bool) -> (a -> a) -> a -> a
applyWhen' cond f x = if cond x then f x else x

traceShowWith :: Show a => (b -> a) -> b -> b
traceShowWith f v = traceShow (f v) v
{- General helper functions -}

{- Lens helpers -}
-- byName :: NameTag a => Identifier -> Lens' [AST.Class] AST.Class
-- byName n f cs = applyWhen ((n ==) . name) f <$> cs

cIassesL :: Lens' GlobalCtx [AST.Class]
cIassesL f (GlobalCtx a b) = f a <&> \a' -> GlobalCtx a' b

cextendsL :: Lens' AST.Class (Maybe Identifier)
cextendsL fn (AST.Class a b c d e f) = fn c <&> \c' -> AST.Class a b c' d e f
cconstructorsL :: Lens' AST.Class [AST.Constructor]
cconstructorsL fn (AST.Class a b c d e f) = fn f <&> AST.Class a b c d e

crbodyL :: Lens' AST.Constructor AST.Stmt
crbodyL f (AST.Constructor a b c d) = f d <&> AST.Constructor a b c

localsL :: Lens' ExprCtx (Map Identifier Type)
localsL f (Ctx a b c) = f a <&> \a' -> Ctx a' b c
{- Lens helper end -}

data GlobalCtx = GlobalCtx { cIasses :: [AST.Class], stdLib :: [AST.Class] }
type ExceptState = ExceptT String (State GlobalCtx)

liftBool :: MonadError e m => e -> Bool -> m ()
liftBool _   True  = return ()
liftBool err False = throwError err
liftBoolM :: MonadError e m => e -> m Bool -> m ()
liftBoolM err =  (>>= liftBool err)
liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe _ (Just x)  = return x
liftMaybe err Nothing = throwError err
liftMaybeM :: MonadError e m => e -> m (Maybe a) -> m a
liftMaybeM err = (>>= liftMaybe err)

data AccessCtx = AccessCtx { astatic :: Bool, aname :: Identifier }
  deriving (Show)
instance StaticTag   AccessCtx where static = astatic
instance NameTag     AccessCtx where name = aname
instance From        AccessCtx ExprCtx where
  from exprCtx = AccessCtx { astatic = static exprCtx, aname = AST.cname $ cIass exprCtx}
instance FromPartial AccessCtx TAST.Expr String where
  fromM expr' = case typee expr' of
    (Instance cn) -> return AccessCtx { astatic=False, aname=cn }
    (Class    cn) -> return AccessCtx { astatic=True , aname=cn }
    _             -> throwError $ "Expected " ++ show expr' ++ " to have to be a class or class instance."
-- Alias for better understandibility
accessCtx :: From AccessCtx a => a -> AccessCtx
accessCtx = from
accessCtxM :: FromPartial AccessCtx a String => a -> ExceptState AccessCtx
accessCtxM = fromM

showSignature :: Identifier -> [Type] -> String
showSignature fname partypes = fname ++ "(" ++ intercalate ", " (show <$> partypes) ++ ")"

isBlock :: AST.Stmt -> Bool
isBlock AST.Block{} = True
isBlock _           = False

data ExprCtx = Ctx { locals :: Map Identifier Type, cIass :: AST.Class, staticCtx :: Bool}
instance StaticTag ExprCtx where static = staticCtx

dummyPosition :: AST.Position
dummyPosition  = AST.Position{ AST.start = (0,0), AST.end = (0,0) }
defSuperCall :: AST.Stmt
defSuperCall = AST.SuperCall dummyPosition []
defConstructor :: AST.Class -> AST.Constructor
defConstructor cIass = AST.Constructor (access cIass) (name cIass) [] $ AST.Block dummyPosition [defSuperCall]

typecheck :: AST.Program -> TAST.Program
typecheck prg = case flip evalState globalCtx $ runExceptT $ typecheckM prg of
  Left err   -> error err
  Right prg' -> prg'
  where globalCtx = GlobalCtx { cIasses = prg, stdLib = StdLib.stdLib }

typecheckM :: AST.Program -> ExceptState TAST.Program
typecheckM prg = do
  prechecked <- precheckProgram prg
  cIassesL .= prechecked
  checkProgram prechecked

{- Prechecks
 - Check invariants that are later needed to not throw errors at the wrong positions
 - Expand AST by libararies and missing constructors that are later expected to exist

 - Prechecks of classes are generally independent from each other,
 - while checks need information from other classes.
 -}
precheckProgram :: AST.Program -> ExceptState AST.Program
precheckProgram prg = do
  liftBool "Classes must have unique names."
    $ hasNoDuplicates $ AST.cname <$> prg
  mapM precheckClass prg

precheckClass :: AST.Class -> ExceptState AST.Class
precheckClass cIass = do
  liftBoolM (name cIass ++ " redefines a standard library class.")
    $ all ((name cIass /=) . simpleName) <$> gets stdLib
  precheckExtends cIass
  cIass & cconstructorsL %~ applyWhen' null (defConstructor cIass:)
        & cextendsL %~ applyWhen' isNothing (const $ Just "java/lang/Object")
        & cconstructorsL . traverse %%~ precheckConstructor
  where precheckExtends = void . anchestors

precheckConstructor :: AST.Constructor -> ExceptState AST.Constructor
precheckConstructor cr = do
  liftBool ("Body of constructor " ++ showSignature (name cr) (ptypes cr) ++ " must be a block")
    $ isBlock (AST.crbody cr)
  let cr' = cr & crbodyL . stmtsL %~ applyWhen' (not . maybe False isCrCall . listToMaybe) (defSuperCall:)
  return cr'
  where isCrCall (AST.ThisCall{})  = True
        isCrCall (AST.SuperCall{}) = True
        isCrCall _                 = False
        -- Used only after checking a block exists
        stmtsL f (AST.Block a b) = f b <&> AST.Block a

checkProgram :: AST.Program -> ExceptState TAST.Program
checkProgram prg = do
  mapM checkClass prg

checkClass :: AST.Class -> ExceptState TAST.Class
checkClass cIass@AST.Class{AST.caccess, AST.cname, AST.cextends, AST.cfields, AST.cmethods, AST.cconstructors} = do
  liftBool ("Accessibility of top level class " ++ cname ++ " must be public or package.")
    $ caccess == Public || caccess == Package
  liftBool ("Fields must have unique names in class " ++ cname)
    $ hasNoDuplicates (name <$> cfields)
  forM_ (safeTails cmethods) $ \(reference,subjects) ->
    forM_ subjects $ checkDuplicateMethod reference
  cfields' <- mapM (checkField cIass) cfields
  cmethods' <- mapM (checkMethod cIass) cmethods
  cconstructors' <- mapM (checkConstructor cIass) cconstructors
  return TAST.Class {
    TAST.caccess,
    TAST.cname,
    TAST.cextends = cextends,
    TAST.cfields = cfields',
    TAST.cmethods = cmethods',
    TAST.cconstructors = cconstructors'
  }
  where safeTails = mapMaybe uncons . tails

checkField :: AST.Class -> AST.Field -> ExceptState TAST.Field
checkField cIass AST.Field { AST.faccess, AST.fstatic, AST.ftype, AST.fname, AST.finit } = do
  finit' <- mapM (checkExpr Ctx{locals=Map.empty,cIass,staticCtx=fstatic}) finit
  return TAST.Field {
    TAST.faccess,
    TAST.ftype,
    TAST.fstatic,
    TAST.fname,
    TAST.finit = finit'
  }

checkMethod :: AST.Class -> AST.Method -> ExceptState TAST.Method
checkMethod cIass method@AST.Method{ AST.moverride, AST.maccess, AST.mstatic, AST.mtype, AST.mname, AST.mparams, AST.mbody } = do
  liftBool ("Parameters must have unique names in method " ++ AST.cname cIass ++ "." ++ mname ++ ".")
    $ hasNoDuplicates (snd <$> mparams)
  liftBool ("Body of method " ++ name cIass ++ "." ++ showSignature mname (fst <$> mparams) ++ " must be a block")
    $ isBlock mbody
  (mbody', defReturn) <- checkStmt Ctx{locals,cIass,staticCtx=mstatic} mtype mbody
  liftBool ("Not all paths return a value in method " ++ AST.cname cIass ++ "." ++ mname ++ ".")
    $ mtype == Void || defReturn
  moverridden <- findOverridden (AST.cextends cIass)
  when moverride
    $ liftBool ("Method " ++ AST.cname cIass ++ "." ++ AST.mname method ++ " is not overriding any method.")
    $ isJust moverridden
  whenJust moverridden $ \(overCIass, overridden) -> do
    liftBool ("The return type of method " ++ AST.cname cIass ++ "." ++ AST.mname method ++ "  must be a subtype of the overridden method " ++ AST.cname overCIass ++ "." ++ AST.mname overridden ++ ".")
      =<< AST.mtype method <: AST.mtype overridden
    liftBool ("The access modifier of method " ++ AST.cname cIass ++ "." ++ AST.mname method ++ "  must not be stricter than the overridden method " ++ AST.cname overCIass ++ "." ++ AST.mname overridden ++ ".")
      $ AST.maccess method <= AST.maccess overridden
  return TAST.Method {
    TAST.maccess,
    TAST.mstatic,
    TAST.mtype,
    TAST.mname,
    TAST.mparams,
    TAST.mbody = mbody'
  }
  where locals = Map.fromList $ swap <$> mparams
        -- Search for overridden method in superclasses
        findOverridden :: Maybe Identifier -> ExceptState (Maybe (AST.Class, AST.Method))
        findOverridden Nothing = return Nothing
        findOverridden (Just cIassName) = do
          cIass <- resolveClass cIassName
          let moverridden = flip find (AST.cmethods cIass)$ \m ->
                name m == name method
                && ptypes m == ptypes method
                && access m < Private -- private methods are just not visible
          return ((cIass,) <$> moverridden) <<>^ findOverridden (AST.cextends cIass)

checkConstructor :: AST.Class -> AST.Constructor -> ExceptState TAST.Constructor
checkConstructor cIass cr@AST.Constructor{ AST.craccess, AST.crparams, AST.crbody } = do
  liftBool ("Parameters must have unique names in constructor " ++ showSignature (name cIass) (ptypes cr) ++ ".")
    $ hasNoDuplicates (pnames cr)
  (crbody',_) <- checkStmt Ctx{locals,cIass,staticCtx=False} Void crbody
  checkReturn crbody -- instead of defReturn check
  return TAST.Constructor {
    TAST.craccess,
    TAST.crparams,
    TAST.crbody = crbody'
  }
  where locals = Map.fromList $ swap <$> crparams
        checkReturn :: AST.Stmt -> ExceptState ()
        checkReturn (AST.Block _ stmts) = forM_ stmts checkReturn
        checkReturn (AST.Return pos mexpr) = when (isJust mexpr) $ throwError ("Unexpected return value at " ++ show pos)
        checkReturn (AST.While _ _ stmt) = checkReturn stmt
        checkReturn (AST.LocalVarDecl {}) = return ()
        checkReturn (AST.If _ _ stmt1 mstmt2) = checkReturn stmt1 >> forM_ mstmt2 checkReturn
        checkReturn (AST.ThisCall _ _) = return ()
        checkReturn (AST.SuperCall _ _) = return ()
        checkReturn (AST.StmtOrExprAsStmt _ _) = return ()

checkStmt :: ExprCtx -> {-target::-}Type -> AST.Stmt -> ExceptState (TAST.Stmt, {-definiteReturn::-}Bool)
checkStmt _   target (AST.Block _ []) =
  if target == Void
  then return (TAST.Block [TAST.Return Nothing], True)
  else return (TAST.Block [], False)
checkStmt ctx target (AST.Block blockPos (AST.LocalVarDecl _ ltype lname minit:stmts)) = do
  minit' <- mapM (checkExpr ctx) minit
  whenJust minit' $ \init' ->
    liftBool ("Expected initializer " ++ show (fromJust minit) ++ " to have declared type " ++ show ltype)
    =<< typee init' <: ltype
  (block', defReturns) <- checkStmt (ctx & localsL %~ Map.insert lname ltype) target $ AST.Block blockPos stmts
  --  This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  return (TAST.Block (TAST.LocalVarDecl ltype lname minit':stmts'), defReturns)
checkStmt ctx target (AST.Block blockPos (stmt:stmts)) = do
  (stmt', defReturn) <- checkStmt ctx target stmt
  if defReturn
  then do
    liftBool ("Unreachable code: " ++ show stmts ++ ".")
      $ null stmts
    return (TAST.Block [stmt'], True)
  else do
    (block', defReturns) <- checkStmt ctx target $ AST.Block blockPos stmts
    --  This is safe, as AST.Block is used as input
    let (TAST.Block stmts') = block'
    return (TAST.Block (stmt':stmts'), defReturn || defReturns)
checkStmt ctx target (AST.Return pos mexpr) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  whenNothing mexpr'
    $ liftBool ("Missing return value at " ++ show pos)
    $ Void == target
  whenJust mexpr' $ \expr' ->
    liftBool ("Expected " ++ show expr' ++ " to match return type " ++ show target ++ ".")
    =<< typee expr' <: target
  return (TAST.Return mexpr', True)
checkStmt ctx target (AST.While _ cond body) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected while condition " ++ show cond' ++ " to have type bool")
    $ typee cond' == Bool
  (body', _) <- checkStmt ctx target body
  return (TAST.While cond' body', False)
checkStmt _   _      decl@(AST.LocalVarDecl {}) =
  throwError $ "Variable declaration " ++ show decl ++ " is not allowed here."
checkStmt ctx target (AST.If _ cond ifBranch melseBranch) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected if condition " ++ show cond' ++ " to have type bool")
    $ typee cond' == Bool
  (ifBranch', defReturn1) <- checkStmt ctx target ifBranch
  (melseBranch', mdefReturn2) <- unzip <$> mapM (checkStmt ctx target) melseBranch
  return (TAST.If cond' ifBranch' melseBranch', defReturn1 && fromMaybe False mdefReturn2)
checkStmt ctx _      (AST.ThisCall _ args)  = do
  args' <- mapM (checkExpr ctx) args
  let cname = name $ cIass ctx
  constructor <- resolveConstructor ctx cname (typee <$> args')
  return (TAST.ThisCall cname $ zip (ptypes constructor) args', False)
checkStmt ctx _      (AST.SuperCall pos args) = do
  let cname = name $ cIass ctx
  cextends <- liftMaybe ("Class " ++ cname ++ " has no superclass") (AST.cextends $ cIass ctx)
  args' <- mapM (checkExpr ctx) args
  constructor <- resolveConstructor ctx cextends (typee <$> args')
  return (TAST.SuperCall cextends $ zip (ptypes constructor) args', False)
checkStmt ctx _      (AST.StmtOrExprAsStmt pos stmtExpr) = do
  expr' <- checkExpr ctx $ AST.StmtOrExprAsExpr pos stmtExpr
   -- This is safe, as AST.StmtOrExprAsExpr is used as input
  let (TAST.StmtOrExprAsExpr stmtExpr') = expr'
  return (TAST.StmtOrExprAsStmt stmtExpr', False)

checkExpr :: ExprCtx -> AST.Expr -> ExceptState TAST.Expr
checkExpr ctx (AST.This _) = do
  liftBool "this can only be used in a non-static context." $
    not (static ctx)
  return $ TAST.This (Instance $ AST.cname $ cIass ctx)
checkExpr _ (AST.Super _) = throwError "super is currently unsupported."
checkExpr ctx@Ctx{locals, cIass} (AST.Name pos uname) = do
  liftMaybe ("Name " ++ uname ++ " not found at " ++ show pos)
    =<< asLocal <<>^ asField <<>^ asClass
  where
    cIassName = AST.cname cIass
    asLocal = do
      let mtype = Map.lookup uname locals
      forM mtype $ \t ->
        return $ TAST.LocalVar t uname
    asField = do
      mfound <- lookupField ctx (accessCtx ctx) uname
      forM mfound $ \(fieldCIass, field) -> do
        let this' = TAST.This (Instance cIassName)
        return $ TAST.FieldAccess (AST.ftype field) this' fieldCIass (static field) uname
    asClass = do
      mcIass <- lookupClass uname
      forM mcIass $ \c ->
        return $ TAST.ClassRef (Class $ AST.cname c) (AST.cname c)
checkExpr ctx (AST.FieldAccess _ expr fname) = do
  expr' <- checkExpr ctx expr
  accCtx <- accessCtxM expr'
  (fieldCIass, field) <- resolveField ctx accCtx fname
  return $ TAST.FieldAccess (AST.ftype field) expr' fieldCIass (static field) fname
checkExpr ctx (AST.Unary _ op expr) = do
  expr' <- checkExpr ctx expr
  tresult <- liftMaybeM ("Operator " ++ show op ++ " is not defined on type " ++ show (typee expr') ++ ".")
    $ (<$$>) snd
    $ flip findM types $ \((op', tin),_) -> return (op' == op) &&^ (typee expr' <: tin)
  return $ TAST.Unary tresult op expr'
    where types =
            [ ((Plus        , Int ), Int )
            , ((Minus       , Int ), Int )
            , ((LNot        , Bool), Bool)
            ]
checkExpr ctx (AST.Binary _ op expr1 expr2) = do
  expr1' <- checkExpr ctx expr1
  expr2' <- checkExpr ctx expr2
  tresult <- liftMaybeM ("Operator " ++ show op ++ " is not defined on type " ++ show (typee expr1') ++ " x " ++ show (typee expr2') ++ ".")
    $ (<$$>) snd
    $ flip findM types $ \((op', tin1, tin2),_) -> return (op' == op) &&^ (typee expr1' <: tin1) &&^ (typee expr2' <: tin2)
  return $ TAST.Binary tresult op expr1' expr2'
    where types =
            [ ((Add, Int, Int), Int)
            , ((Sub, Int, Int), Int)
            , ((Mul, Int, Int), Int)
            , ((Div, Int, Int), Int)
            , ((Mod, Int, Int), Int)
            , ((LAnd, Bool, Bool), Bool)
            , ((LOr, Bool, Bool), Bool)
            , ((LT, Int, Int), Bool)
            , ((LTE, Int, Int), Bool)
            , ((GT, Int, Int), Bool)
            , ((GTE, Int, Int), Bool)
            , ((EQ, Int, Int), Bool)
            , ((EQ, Bool, Bool), Bool)
            , ((EQ, Char, Char), Bool)
            , ((EQ, Instance "Object", Instance "Object"), Bool)
            , ((NEQ, Int, Int), Bool)
            , ((NEQ, Bool, Bool), Bool)
            , ((NEQ, Char, Char), Bool)
            , ((NEQ, Instance "Object", Instance "Object"), Bool)
            ]
checkExpr _ (AST.Literal _ (AST.IntLit i)) = return $ TAST.Literal Int $ TAST.IntLit i
checkExpr _ (AST.Literal _ (AST.CharLit c)) = return $ TAST.Literal Char $ TAST.CharLit c
checkExpr _ (AST.Literal _ (AST.BoolLit b)) = return $ TAST.Literal Bool $ TAST.BoolLit b
checkExpr _ (AST.Literal _ AST.Null) = return $ TAST.Literal NullType TAST.Null
checkExpr ctx@Ctx{locals} (AST.StmtOrExprAsExpr pos (AST.Assign mleft uname right)) = do
  case mleft of
    Just left -> asExpr left
    Nothing   -> liftMaybe ("Local or field variable " ++ uname ++ " not found at " ++ show pos)
                 =<< asLocal <<>^ asField
  where
    checkAssignable target right' =
      liftBool ("Type " ++ show (typee right') ++ " is not assignable to type " ++ show target ++ " at " ++ show pos ++ ".")
      =<< typee right' <: target
    asExpr left = do
      left' <- checkExpr ctx left
      right' <- checkExpr ctx right
      accCtx <- accessCtxM left'
      (fieldCIass, field) <- resolveField ctx accCtx uname
      checkAssignable (AST.ftype field) left'
      return $ TAST.StmtOrExprAsExpr $ TAST.FieldAssign (typee field) left' fieldCIass (static field) uname right'
    asLocal = do
      right' <- checkExpr ctx right
      let mltype = Map.lookup uname locals
      forM mltype $ \ltype -> do
        checkAssignable ltype right'
        return $ TAST.StmtOrExprAsExpr $ TAST.LocalAssign ltype uname right'
    asField = do
      mfound <- lookupField ctx (accessCtx ctx) uname
      right' <- checkExpr ctx right
      forM mfound $ \(fieldCIass, field) -> do
        checkAssignable (AST.ftype field) right'
        let this' = TAST.This (Instance $ AST.cname $ cIass ctx)
        return $ TAST.StmtOrExprAsExpr $ TAST.FieldAssign (typee field) this' fieldCIass (static field) uname right'
checkExpr ctx (AST.StmtOrExprAsExpr _ (AST.New cname args)) = do
  args' <- mapM (checkExpr ctx) args
  constructor <- resolveConstructor ctx cname (typee <$> args')
  return
    $ TAST.StmtOrExprAsExpr
    $ TAST.New (Instance cname) cname $ zip (ptypes constructor) args'
checkExpr ctx (AST.StmtOrExprAsExpr _ (AST.MethodCall mexpr mname args)) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  let expr' = flip fromMaybe mexpr' $ if static ctx
      then TAST.ClassRef (Class cIassName) cIassName
      else TAST.This (Instance cIassName)
  args' <- mapM (checkExpr ctx) args
  accCtx <- accessCtxM expr'
  (methodCIass, method) <- resolveMethod ctx accCtx mname (typee <$> args')
  return
    $ TAST.StmtOrExprAsExpr
    $ TAST.MethodCall (typee method) expr' methodCIass (static method) mname $ zip (ptypes method) args'
  where cIassName = AST.cname $ cIass ctx

{- typecheck helper functions
-}
-- doesn't need monad, because all classes are public/package and therefore accessible
lookupClass :: Identifier -> ExceptState (Maybe AST.Class)
lookupClass cname = asStdLib <<>^ asCIass
  where asStdLib = find (((cname ==) . simpleName) ||| (cname ==) . name) <$> gets stdLib
        asCIass  = find ((cname ==) . name) <$> gets cIasses
resolveClass :: Identifier -> ExceptState AST.Class
resolveClass cname = liftMaybe ("Class " ++ cname ++ " does not exist.") =<< lookupClass cname

lookupMethod :: ExprCtx -> AccessCtx -> Identifier -> [Type] -> ExceptState (Maybe (Identifier, AST.Method))
lookupMethod exprCtx accCtx mname targs = do
  mfound <- lookupMethod' (name accCtx)
  whenJust mfound $ checkStatic accCtx . snd
  return mfound
  where
    lookupMethod' aname = do
      cIass@AST.Class{AST.cname=cIassName} <- resolveClass aname
      minheritedMethod <- fmap join $ mapM lookupMethod' $ AST.cextends cIass
      foldlM (chooseMostSpecific cIassName) minheritedMethod
        =<< filterM (isAccessible exprCtx cIass)
        =<< filterM (`isApplicableTo` targs)
        (   filter  (hasName mname)
        $   AST.cmethods cIass)
resolveMethod :: ExprCtx -> AccessCtx -> Identifier -> [Type] -> ExceptState (Identifier, AST.Method)
resolveMethod exprCtx accCtx mname targs =
  liftMaybe ("No accessible method " ++ showSignature mname targs ++ " found on class " ++ name accCtx ++ ".")
  =<< lookupMethod exprCtx accCtx mname targs

lookupConstructor :: ExprCtx -> Identifier -> [Type] -> ExceptState (Maybe AST.Constructor)
lookupConstructor ctx cIassName targs = do
  cIass <- resolveClass cIassName
  mfound <- foldlM (chooseMostSpecific cIassName) Nothing
    =<< filterM (isAccessible ctx cIass)
    =<< filterM (`isApplicableTo` targs)
    (   AST.cconstructors cIass)
  return $ snd <$> mfound
resolveConstructor :: ExprCtx -> Identifier -> [Type] -> ExceptState AST.Constructor
resolveConstructor ctx cIassName targs =
  liftMaybe ("No accessible constructor " ++ showSignature cIassName targs ++ " found on class " ++ cIassName ++ ".")
  =<< lookupConstructor ctx cIassName targs

chooseMostSpecific :: (ParamsTag p, NameTag p) => Identifier -> Maybe (Identifier, p) -> p -> ExceptState (Maybe (Identifier, p))
chooseMostSpecific cIassName Nothing         m2 =
  return $ Just (cIassName, m2)
chooseMostSpecific cIassName (Just (cN1,m1)) m2 = condM
  [(ptypes m1 =~: ptypes m2, return $ Just (cIassName, m2)                                  )
  ,(ptypes m1 <~: ptypes m2, return $ Just (cN1, m1)                                        )
  ,(ptypes m2 <~: ptypes m1, return $ Just (cIassName, m2)                                  )
  ,(otherwiseM               , throwError $ "Reference to " ++ name m1 ++ " is ambiguous")
  ]
  where (=~:) = return .* (==)

lookupField :: ExprCtx -> AccessCtx -> Identifier -> ExceptState (Maybe (Identifier, AST.Field))
lookupField exprCtx accCtx fname = do
  cIass <- resolveClass (name accCtx)
  mfield <- listToMaybe <$>
          ( filterM (isAccessible exprCtx cIass)
          $ filter (hasName fname)
          $ AST.cfields cIass)
  whenJust mfield $ checkStatic accCtx
  return $ (AST.cname cIass,) <$> mfield
resolveField :: ExprCtx -> AccessCtx -> Identifier ->ExceptState (Identifier, AST.Field)
resolveField exprCtx accCtx fname =
  liftMaybe ("No accessible field " ++ fname ++ " found on class " ++ name accCtx ++ ".")
  =<< lookupField exprCtx accCtx fname

hasName :: NameTag n => Identifier -> n -> Bool
hasName n = (n ==) . name

isAccessible :: AccessTag a => ExprCtx -> AST.Class -> a -> ExceptState Bool
isAccessible Ctx{cIass=this} target a = case access a of
  Private   -> return $ tthis == ttarget
  Protected ->          tthis <: ttarget
  _         -> return   True
  where tthis   = Instance (AST.cname this)
        ttarget = Instance (AST.cname target)

checkStatic :: StaticTag s => AccessCtx -> s -> ExceptState ()
checkStatic ctx s =
  when (static ctx)
    $ liftBool "Cannot use non-static method/field in a static context."
    $ static s

checkDuplicateMethod :: AST.Method -> AST.Method -> ExceptState ()
checkDuplicateMethod m1 m2 =
  when (AST.mname m1 == AST.mname m2)
    $ liftBool ("Method " ++ showSignature (name m1) (fst <$> AST.mparams m1) ++ " is already defined.")
    $ (fst <$> AST.mparams m1) /= (fst <$> AST.mparams m2)

{- type related functions
-}
anchestors :: AST.Class -> ExceptState [Identifier]
anchestors cIass = anchestors' (AST.cextends cIass) []
  where anchestors' :: Maybe Identifier -> [Identifier] -> ExceptState [Identifier]
        anchestors' Nothing found = return $ reverse found
        anchestors' (Just superName) found = do
          when (superName `elem` found) $
            throwError $ "Detected inheritance cycle: " ++ show found ++ "."
          super <- resolveClass superName
          anchestors' (AST.cextends super) (AST.cname super : found)

isApplicableTo :: ParamsTag p => p -> [Type] -> ExceptState Bool
method `isApplicableTo` targs = matchingLength &&^ matchingTypes
  where matchingLength = return $ length (params method) == length targs
        matchingTypes  = and <$> zipWithM (<:) targs (ptypes method)

-- Return whether parameter types of one method are a specialization of another
(<~:) :: [Type] -> [Type] -> ExceptState Bool
(<~:) tparams1 tparams2 = and <$> zipWithM (<:) tparams1 tparams2

(<:) :: Type -> Type -> ExceptState Bool
Instance _  <: Instance "Object" = return True
Instance c1 <: Instance c2 =
  return (c1 == c2) ||^ (c2 `elem`) <$> (anchestors =<< resolveClass c1)
t1 <: t2                         = return $ t1 == t2
