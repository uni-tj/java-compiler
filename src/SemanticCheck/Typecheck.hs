{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module SemanticCheck.Typecheck(typecheck) where

import           Control.Composition  ((.*))
import           Control.Lens         (Lens', (%%~), (%~), (.=))
import           Control.Monad.Except (ExceptT, MonadError (throwError), forM,
                                       join, runExceptT, unless, void, zipWithM)
import           Control.Monad.Extra  (filterM, findM, forM_, ifM, unlessM,
                                       when, whenJust, whenM, (&&^), (||^))
import           Control.Monad.State  (State, evalState, gets)
import           Data.Bool            (bool)
import           Data.Foldable        (foldlM)
import           Data.Function        ((&))
import           Data.Functor         ((<&>))
import           Data.Functor.Syntax  ((<$$>))
import           Data.List            (find, groupBy, intercalate)
import           Data.List.NonEmpty   (unzip)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust, fromMaybe, isJust, isNothing,
                                       listToMaybe)
import           Data.Tuple.Extra     (swap)
import           Debug.Trace          (traceShow)
import           Error.PrintError     (File, FileContainer (file), throwPretty)
import           GHC.Data.Maybe       (firstJust)
import           Prelude              hiding (EQ, GT, LT, init, showList, unzip)
import qualified SemanticCheck.StdLib as StdLib
import           SemanticCheck.Util
import qualified Types.AST            as AST
import           Types.Core           (AccessModifier (..), BinOperator (..),
                                       Identifier, Type (..), UnOparator (..))
import qualified Types.TAST           as TAST

{- General helper functions -}
duplicatesWith :: Eq b => (a -> b) -> [a] -> [[a]]
duplicatesWith f = filter ((2 <=) . length) . groupBy (\x y -> f x == f y)

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

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing m = whenNothingM (return m)
whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM m alt = m >>= \case
  (Just x) -> return x
  Nothing  -> alt

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip (bool id)
applyWhen' :: (a -> Bool) -> (a -> a) -> a -> a
applyWhen' cond f x = if cond x then f x else x

traceShowWith :: Show a => (b -> a) -> b -> b
traceShowWith f v = traceShow (f v) v
{- General helper functions end -}

{- Lens helpers -}
-- byName :: NameTag a => Identifier -> Lens' [AST.Class] AST.Class
-- byName n f cs = applyWhen ((n ==) . name) f <$> cs

cIassesL :: Lens' GlobalCtx [AST.Class]
cIassesL f (GlobalCtx a b c) = f a <&> \a' -> GlobalCtx a' b c

cextendsL :: Lens' AST.Class (Maybe (AST.WithPosition Identifier))
cextendsL fn (AST.Class a b c d e f g) = fn d <&> \d' -> AST.Class a b c d' e f g
cconstructorsL :: Lens' AST.Class [AST.Constructor]
cconstructorsL fn (AST.Class a b c d e f g) = fn g <&> AST.Class a b c d e f

crbodyL :: Lens' AST.Constructor AST.Stmt
crbodyL f (AST.Constructor a b c d e) = f e <&> AST.Constructor a b c d

localsL :: Lens' ExprCtx (Map Identifier Type)
localsL f (Ctx a b c) = f a <&> \a' -> Ctx a' b c

-- Partial lens on the statements of a block
stmtsL :: Lens' TAST.Stmt [TAST.Stmt]
stmtsL fn (TAST.Block a) = fn a <&> TAST.Block
{- Lens helpers end -}

data GlobalCtx = GlobalCtx { cIasses :: [AST.Class], stdLib :: [AST.Class], gfile :: File }
instance FileContainer GlobalCtx where file = gfile
type ExceptState = ExceptT String (State GlobalCtx)

data AccessCtx = AccessCtx { astatic :: Bool, aname :: Identifier }
  deriving (Show)
instance StaticTag   AccessCtx where static = astatic
instance NameTag     AccessCtx where name = aname
instance From        AccessCtx ExprCtx where
  from exprCtx = AccessCtx { astatic = static exprCtx, aname = name $ cIass exprCtx}
instance FromPartial AccessCtx TAST.Expr String where
  fromM expr' = case typee' expr' of
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

showList :: Show a => [a] -> String
showList = intercalate ", " . map show

throwInternal :: (MonadError String m) => String -> m a
throwInternal = throwError . ("Internal Error: " ++)

isBlock :: AST.Stmt -> Bool
isBlock AST.Block{} = True
isBlock _           = False

infixl 9 @
(@) :: a -> AST.Position -> AST.WithPosition a
(@) = flip AST.WithPosition

data ExprCtx = Ctx { locals :: Map Identifier Type, cIass :: AST.Class, staticCtx :: Bool}
instance StaticTag ExprCtx where static = staticCtx

defSuperCall :: AST.Stmt
defSuperCall = AST.SuperCall AST.AutoGenerated []
defConstructor :: AST.Class -> AST.Constructor
defConstructor cIass
  = AST.Constructor AST.AutoGenerated (StdLib.withAuthGen $ access cIass) (StdLib.withAuthGen $ name cIass) []
  $ AST.Block AST.AutoGenerated [defSuperCall]

typecheck :: File -> AST.Program -> TAST.Program
typecheck _file prg = case flip evalState globalCtx $ runExceptT $ typecheckM prg of
  Left err   -> error err
  Right prg' -> prg'
  where globalCtx = GlobalCtx { cIasses = prg, stdLib = StdLib.stdLib, gfile = _file }

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
  let duplicateNames = duplicatesWith (untag :: _ -> String) $ tname <$> prg
  whenJust (listToMaybe duplicateNames)
    $ throwPretty "Duplicate class name." . position . last
  precheckTypes prg
  mapM precheckClass prg

precheckClass :: AST.Class -> ExceptState AST.Class
precheckClass cIass = do
  whenM (any ((name cIass ==) . simpleName) <$> gets stdLib)
    $ throwPretty (name cIass ++ " redefines a standard library class.") $ position (tname cIass)
  precheckExtends cIass
  cIass & cconstructorsL %~ applyWhen' null (defConstructor cIass:)
        & cextendsL %~ applyWhen' isNothing (const $ Just $ StdLib.withAuthGen "java/lang/Object")
        & cconstructorsL . traverse %%~ precheckConstructor
  where precheckExtends = void . anchestors

precheckConstructor :: AST.Constructor -> ExceptState AST.Constructor
precheckConstructor cr = do
  checkBodyIsBlock cr
  let cr' = cr & crbodyL . stmtsL %~ applyWhen' (not . maybe False isCrCall . listToMaybe) (defSuperCall:)
  return cr'
  where isCrCall (AST.ThisCall{})  = True
        isCrCall (AST.SuperCall{}) = True
        isCrCall _                 = False
        -- Used only after checking a block exists
        stmtsL f (AST.Block a b) = f b <&> AST.Block a

{- Ensure every user-writte type is valid -}
precheckTypes :: AST.Program -> ExceptState ()
precheckTypes prg = do
  forM_ prg $ \cIass -> do
    forM_ (AST.cfields cIass) $ checkTypeValid . ttypee
    forM_ (AST.cmethods cIass) $ \method -> do
      checkTypeValid (ttypee method)
      forM_ (tptypes method) checkTypeValid
      traverseStmtM precheckTypesStmt (body method)
    forM_ (AST.cconstructors cIass) $ \constr -> do
      forM_ (tptypes constr) checkTypeValid
      traverseStmtM precheckTypesStmt (body constr)
  where
    precheckTypesStmt (AST.LocalVarDecl pos _type _ _) = checkTypeValid $ AST.WithPosition pos _type
    precheckTypesStmt _ = return ()
-- precheckTypes

checkProgram :: AST.Program -> ExceptState TAST.Program
checkProgram prg = do
  mapM checkClass prg

checkClass :: AST.Class -> ExceptState TAST.Class
checkClass cIass@AST.Class{ AST.cfields, AST.cmethods, AST.cconstructors } = do
  when (access cIass > Package)
    $ throwPretty ("Accessibility of top level class " ++ name cIass ++ " must be public or package.") $ position (taccess cIass)
  whenJust (listToMaybe $ duplicatesWith (untag :: _ -> String) $ tname <$> cfields) $
      throwPretty "Duplicate field name." . position . last
  checkDuplicateSignature "method" cmethods
  checkDuplicateSignature "constructor" cconstructors
  cfields' <- mapM (checkField cIass) cfields
  cmethods' <- mapM (checkMethod cIass) cmethods
  cconstructors' <- mapM (checkConstructor cIass) cconstructors
  return TAST.Class {
    TAST.caccess = access cIass,
    TAST.cname = name cIass,
    TAST.cextends = extends cIass,
    TAST.cfields = cfields',
    TAST.cmethods = cmethods',
    TAST.cconstructors = cconstructors'
  }

checkField :: AST.Class -> AST.Field -> ExceptState TAST.Field
checkField cIass field@AST.Field{ AST.finit } = do
  finit' <- forM finit $ checkExpr Ctx{locals=Map.empty,cIass,staticCtx=static field}
  return TAST.Field {
    TAST.faccess = access field,
    TAST.ftype = typee field,
    TAST.fstatic = static field,
    TAST.fname = name field,
    TAST.finit = finit'
  }

checkMethod :: AST.Class -> AST.Method -> ExceptState TAST.Method
checkMethod cIass method@AST.Method{ AST.moverride, AST.mbody } = do
  checkDuplicateParName method
  checkBodyIsBlock method
  (mbody', defReturn) <- checkStmt Ctx{locals,cIass,staticCtx=static method} (typee method) mbody
  unless (typee method == Void || defReturn)
    $ throwPretty "Some paths are lacking a return statement." $ position mbody
  moverridden <- findOverridden (extends cIass)
  when (untag moverride && static method)
    $ throwPretty "Static methods cannot be annotated with @Override." $ position moverride
  when (untag moverride && isNothing moverridden)
    $ throwPretty "Method does not override a method from a supertype" $ position moverride
  whenJust moverridden $ \(overCIass, overridden) -> do
    unlessM (typee method <: typee overridden)
      $ throwPretty ("Canont override method from class " ++ name overCIass ++ ", since the return type " ++ show (typee method) ++ " is not a suptype of " ++ show (typee overridden) ++ ".")
      $ position (ttypee method)
    when (access method > access overridden)
      $ throwPretty ("Canont override method from class " ++ name overCIass ++ ", since the access modifier too strict.") $ position (taccess method)
  return TAST.Method {
    TAST.maccess = access method,
    TAST.mstatic = static method,
    TAST.mtype = typee method,
    TAST.mname =  name method,
    TAST.mparams = params method,
    TAST.mbody = mbody' & stmtsL %~ applyWhen (typee method == Void) (++[TAST.Return Nothing])
  }
  where locals = Map.fromList $ swap <$> params method
        -- Search for overridden method in superclasses
        findOverridden :: Maybe Identifier -> ExceptState (Maybe (AST.Class, AST.Method))
        findOverridden Nothing = return Nothing
        findOverridden (Just cIassName) = do
          cIass <-
            flip whenNothingM (throwInternal "Invalid superclass found. This should be prechecked.")
            $ lookupClass cIassName
          let moverridden = flip find (AST.cmethods cIass)$ \m ->
                name m == name method
                && ptypes m == ptypes method
                && access m < Private -- private methods are just not visible
          return ((cIass,) <$> moverridden) <<>^ findOverridden (extends cIass)

checkConstructor :: AST.Class -> AST.Constructor -> ExceptState TAST.Constructor
checkConstructor cIass cr@AST.Constructor{ AST.crbody } = do
  checkDuplicateParName cr
  (crbody',_) <- checkStmt Ctx{locals,cIass,staticCtx=False} Void crbody
  checkReturn crbody -- instead of defReturn check
  return TAST.Constructor {
    TAST.craccess = access cr,
    TAST.crparams = params cr,
    TAST.crbody = crbody' & stmtsL %~ (++[TAST.Return Nothing])
  }
  where locals = Map.fromList $ swap <$> params cr
        checkReturn :: AST.Stmt -> ExceptState ()
        checkReturn (AST.Block _ stmts) = forM_ stmts checkReturn
        checkReturn (AST.Return pos mexpr) = when (isJust mexpr) $ void $ throwPretty "Unexpected return value" pos
        checkReturn (AST.While _ _ stmt) = checkReturn stmt
        checkReturn (AST.LocalVarDecl {}) = return ()
        checkReturn (AST.If _ _ stmt1 mstmt2) = checkReturn stmt1 >> forM_ mstmt2 checkReturn
        checkReturn (AST.ThisCall _ _) = return ()
        checkReturn (AST.SuperCall _ _) = return ()
        checkReturn (AST.StmtOrExprAsStmt _ _) = return ()

checkStmt :: ExprCtx -> {-target::-}Type -> AST.Stmt -> ExceptState (TAST.Stmt, {-definiteReturn::-}Bool)
checkStmt _   _      (AST.Block _ []) =
  return (TAST.Block [], False)
checkStmt ctx target (AST.Block blockPos (AST.LocalVarDecl pos ltype lname minit:stmts)) = do
  when (lname `Map.member` locals ctx)
    $ throwPretty "Duplicate local variable name" pos
  minit' <- forM minit $ \init -> do
    init' <- checkExpr ctx init
    unlessM (typee' init' <: ltype)
      $ throwPretty ("Expected right hand side to be a subtype of " ++ show ltype ++ ", but has type " ++ show (typee' init') ++ ".") $ position init
    return init'
  (block', defReturns) <- checkStmt (ctx & localsL %~ Map.insert lname ltype) target $ AST.Block blockPos stmts
  --  This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  return (TAST.Block $ TAST.LocalVarDecl ltype lname minit':stmts', defReturns)
checkStmt ctx target (AST.Block blockPos (stmt:stmts)) = do
  (stmt', defReturn) <- checkStmt ctx target stmt
  if defReturn
  then do
    unless (null stmts)
      $ throwPretty "Unreachable code." $ AST.Position (AST.start $ position $ head stmts) (AST.end $ position $ last stmts)
    return (TAST.Block [stmt'], True)
  else do
    (block', defReturns) <- checkStmt ctx target $ AST.Block blockPos stmts
    --  This is safe, as AST.Block is used as input
    let (TAST.Block stmts') = block'
    return (TAST.Block (stmt':stmts'), defReturn || defReturns)
checkStmt ctx target (AST.Return pos mexpr) = do
  mexpr' <- forM mexpr $ \expr -> do
    expr' <- checkExpr ctx expr
    unlessM (typee' expr' <: target)
      $ throwPretty ("Type " ++ show (typee' expr') ++ " is not a suptype of expected return type " ++ show target ++ ".") $ position expr
    return expr'
  unless (isJust mexpr' || target == Void)
    $ throwPretty "Missing return value." pos
  return (TAST.Return mexpr', True)
checkStmt ctx target (AST.While _ cond block) = do
  cond' <- checkExpr ctx cond
  unlessM (typee' cond' <: Bool)
    $ throwPretty ("While condition must have type bool, but has type " ++ show (typee' cond') ++ ".") $ position cond
  (block', _) <- checkStmt ctx target block
  return (TAST.While cond' block', False)
checkStmt _   _      (AST.LocalVarDecl pos _ _ _) =
  throwPretty "Variable declaration is not allowed here." pos
checkStmt ctx target (AST.If _ cond ifBranch melseBranch) = do
  cond' <- checkExpr ctx cond
  unlessM (typee' cond' <: Bool)
    $ throwPretty ("If condition must have type bool, but has type " ++ show (typee' cond') ++ ".") $ position cond
  (ifBranch', defReturn1) <- checkStmt ctx target ifBranch
  (melseBranch', mdefReturn2) <- unzip <$> mapM (checkStmt ctx target) melseBranch
  return (TAST.If cond' ifBranch' melseBranch', defReturn1 && fromMaybe False mdefReturn2)
checkStmt ctx _      (AST.ThisCall pos args)  = do
  args' <- mapM (checkExpr ctx) args
  let cname = name $ cIass ctx
  constructor <- resolveConstructor ctx pos cname (typee' <$> args')
  return (TAST.ThisCall cname $ zip (ptypes constructor) args', False)
checkStmt ctx _      (AST.SuperCall pos args) = do
  let _cIass = cIass ctx
  superName <- whenNothing (extends $ cIass ctx)
    $ throwPretty ("Class " ++ name (cIass ctx) ++ " has no superclass.") pos
  args' <- mapM (checkExpr ctx) args
  constructor <- resolveConstructor ctx pos superName (typee' <$> args')
  return (TAST.SuperCall superName $ zip (ptypes constructor) args', False)
checkStmt ctx _      (AST.StmtOrExprAsStmt pos stmtExpr) = do
  expr' <- checkExpr ctx $ AST.StmtOrExprAsExpr pos stmtExpr
   -- This is safe, as AST.StmtOrExprAsExpr is used as input
  let (TAST.StmtOrExprAsExpr stmtExpr') = expr'
  return (TAST.StmtOrExprAsStmt stmtExpr', False)

checkExpr :: ExprCtx -> AST.Expr -> ExceptState TAST.Expr
checkExpr ctx (AST.This pos) = do
  when (static ctx)
    $ throwPretty "this ca only be used in a non-static context." pos
  return $ TAST.This (Instance $ name $ cIass ctx)
checkExpr ctx (AST.Super pos) = do
  when (static ctx)
    $ throwPretty "super can only be used in a non-static context." pos
  superName <- whenNothing (extends $ cIass ctx)
    $ throwPretty ("Class " ++ name (cIass ctx) ++ " has no superclass.") pos
  return $ TAST.Super (Instance superName)
checkExpr ctx@Ctx{locals, cIass} (AST.Name pos uname) = do
  whenNothingM (asLocal <<>^ asField <<>^ asClass)
    $ throwPretty "Name not found." pos
  where
    asLocal = do
      let mtype = Map.lookup uname locals
      forM mtype $ \t ->
        return $ TAST.LocalVar t uname
    asField = do
      mfound <- lookupField ctx (accessCtx ctx) uname
      forM mfound $ \(fieldCIass, field) -> do
        let this' = TAST.This (Instance $ name cIass)
        return $ TAST.FieldAccess (typee field) this' fieldCIass (static field) uname
    asClass = do
      mcIass <- lookupClass uname
      forM mcIass $ \c ->
        return $ TAST.ClassRef (Class $ name c) (name c)
checkExpr ctx (AST.FieldAccess pos expr fname) = do
  expr' <- checkExpr ctx expr
  accCtx <- accessCtxM expr'
  (fieldCIass, field) <- resolveField ctx accCtx pos fname
  return $ TAST.FieldAccess (typee field) expr' fieldCIass (static field) fname
checkExpr ctx (AST.Unary pos op expr) = do
  expr' <- checkExpr ctx expr
  tresult <-
    flip whenNothingM (throwPretty ("Operator " ++ show op ++ " is not defined on type " ++ show (typee' expr') ++ ".") pos)
    $ (<$$>) snd
    $ flip findM types $ \((op', tin),_) -> return (op' == op) &&^ (typee' expr' <: tin)
  return $ TAST.Unary tresult op expr'
    where types =
            [ ((Plus        , Int ), Int )
            , ((Minus       , Int ), Int )
            , ((LNot        , Bool), Bool)
            ]
checkExpr ctx (AST.Binary pos op expr1 expr2) = do
  expr1' <- checkExpr ctx expr1
  expr2' <- checkExpr ctx expr2
  tresult <-
    flip whenNothingM (throwPretty ("Operator " ++ show op ++ " is not defined on type " ++ show (typee' expr1') ++ " x " ++ show (typee' expr2') ++ ".") pos)
    $ (<$$>) snd
    $ flip findM types $ \((op', tin1, tin2),_) -> return (op' == op) &&^ (typee' expr1' <: tin1) &&^ (typee' expr2' <: tin2)
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
checkExpr ctx@Ctx{locals} (AST.StmtOrExprAsExpr pos (AST.Assign mleft uname right)) = case mleft of
  Just left -> asExpr left
  Nothing   -> whenNothingM (asLocal <<>^ asField)
                $ throwPretty "Local or field variable not found." pos
  where
    checkAssignable target right' = do
      unlessM (typee' right' <: target)
        $ throwPretty ("Expected right hand side to be a subtype of " ++ show target ++ ", but has type " ++ show (typee' right') ++ ".") $ position right
    asExpr left = do
      left' <- checkExpr ctx left
      right' <- checkExpr ctx right
      accCtx <- accessCtxM left'
      (fieldCIass, field) <- resolveField ctx accCtx pos uname
      checkAssignable (typee field) right'
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
        checkAssignable (typee field) right'
        let this' = TAST.This (Instance $ name $ cIass ctx)
        return $ TAST.StmtOrExprAsExpr $ TAST.FieldAssign (typee field) this' fieldCIass (static field) uname right'
checkExpr ctx (AST.StmtOrExprAsExpr pos (AST.New cname args)) = do
  args' <- mapM (checkExpr ctx) args
  constructor <- resolveConstructor ctx pos cname (typee' <$> args')
  return
    $ TAST.StmtOrExprAsExpr
    $ TAST.New (Instance cname) cname $ zip (ptypes constructor) args'
checkExpr ctx (AST.StmtOrExprAsExpr pos (AST.MethodCall mexpr mname args)) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  let expr' = flip fromMaybe mexpr' $ if static ctx
      then TAST.ClassRef (Class cIassName) cIassName
      else TAST.This (Instance cIassName)
  args' <- mapM (checkExpr ctx) args
  accCtx <- accessCtxM expr'
  (methodCIass, method) <- resolveMethod ctx accCtx pos mname (typee' <$> args')
  return
    $ TAST.StmtOrExprAsExpr
    $ TAST.MethodCall (typee method) expr' methodCIass (static method) mname $ zip (ptypes method) args'
  where cIassName = name $ cIass ctx

{- typecheck helper functions
-}
-- doesn't need monad, because all classes are public/package and therefore accessible
lookupClass :: Identifier -> ExceptState (Maybe AST.Class)
lookupClass cname = asStdLib <<>^ asCIass
  where asStdLib = find (((cname ==) . simpleName) ||| (cname ==) . name) <$> gets stdLib
        asCIass  = find ((cname ==) . name) <$> gets cIasses
resolveClass :: AST.WithPosition Identifier -> ExceptState AST.Class
resolveClass _tcname =
  whenNothingM (lookupClass $ untag _tcname)
    $ throwPretty ("Class " ++ untag _tcname ++ " not found.") $ position _tcname

lookupMethod :: ExprCtx -> AccessCtx -> AST.Position -> Identifier -> [Type] -> ExceptState (Maybe (Identifier, AST.Method))
lookupMethod exprCtx accCtx callSite mname targs = do
  mfound <- lookupMethod' (name accCtx)
  whenJust mfound $ checkStatic "method" accCtx . snd
  return mfound
  where
    lookupMethod' accName = do
      cIass@AST.Class{AST.cname=cIassName} <-
        flip whenNothingM (throwInternal "Found invalid access context.")
        $ lookupClass accName
      minheritedMethod <- fmap join $ mapM lookupMethod' $ extends cIass
      foldlM (chooseMostSpecific callSite (untag cIassName)) minheritedMethod
        =<< filterM (isAccessible exprCtx cIass)
        =<< filterM (`isApplicableTo` targs)
        (   filter  (hasName mname)
        $   AST.cmethods cIass)
resolveMethod :: ExprCtx -> AccessCtx -> AST.Position -> Identifier -> [Type] -> ExceptState (Identifier, AST.Method)
resolveMethod exprCtx accCtx callSite mname targs =
  whenNothingM (lookupMethod exprCtx accCtx callSite mname targs)
    $ throwPretty ("No accessible method " ++ name accCtx ++ "." ++ showSignature mname targs ++ " found.") callSite

lookupConstructor :: ExprCtx -> AST.Position -> Identifier -> [Type] -> ExceptState (Maybe AST.Constructor)
lookupConstructor ctx callSite cIassName targs = do
  cIass <- resolveClass (cIassName @ callSite)
  mfound <- foldlM (chooseMostSpecific callSite cIassName) Nothing
    =<< filterM (isAccessible ctx cIass)
    =<< filterM (`isApplicableTo` targs)
    (   AST.cconstructors cIass)
  return $ snd <$> mfound
resolveConstructor :: ExprCtx -> AST.Position -> Identifier -> [Type] -> ExceptState AST.Constructor
resolveConstructor ctx callSite cIassName targs =
  whenNothingM (lookupConstructor ctx callSite cIassName targs)
    $ throwPretty ("No accessible constructor " ++ showSignature cIassName targs ++ " found.") callSite

chooseMostSpecific :: (ParamsTag m, NameTag m) => AST.Position -> Identifier -> Maybe (Identifier, m) -> m -> ExceptState (Maybe (Identifier, m))
chooseMostSpecific _        cIassName Nothing         m2 =
  return $ Just (cIassName, m2)
chooseMostSpecific callSite cIassName (Just (cN1,m1)) m2 = condM
  [(ptypes m1 =~: ptypes m2, return $ Just (cIassName, m2)                                         )
  ,(ptypes m1 <~: ptypes m2, return $ Just (cN1, m1)                                               )
  ,(ptypes m2 <~: ptypes m1, return $ Just (cIassName, m2)                                         )
  ,(otherwiseM              , throwPretty ("Reference to " ++ name m2 ++ " is ambiguous.") callSite)
  ]
  where (=~:) = return .* (==)

lookupField :: ExprCtx -> AccessCtx -> Identifier -> ExceptState (Maybe (Identifier, AST.Field))
lookupField exprCtx accCtx fname = do
  mfound <- lookupField' (name accCtx)
  whenJust mfound $ checkStatic "field" accCtx . snd
  return mfound
  where
    lookupField' accName = do
      cIass <-
        flip whenNothingM (throwInternal "Found invalid access context.")
        $ lookupClass accName
      minheritedField <- fmap join $ mapM lookupField' $ extends cIass
      mfield <- ((name cIass,) <$$>)
              $ (listToMaybe <$>)
              $ filterM (isAccessible exprCtx cIass)
              $ filter (hasName fname)
              $ AST.cfields cIass
      return $ firstJust mfield minheritedField
resolveField :: ExprCtx -> AccessCtx -> AST.Position -> Identifier ->ExceptState (Identifier, AST.Field)
resolveField exprCtx accCtx callSite fname =
  whenNothingM (lookupField exprCtx accCtx fname)
    $ throwPretty ("No accessible field " ++ name accCtx ++ "." ++ fname ++ " found.") callSite

hasName :: NameTag n => Identifier -> n -> Bool
hasName n = (n ==) . name

isAccessible :: AccessTag a => ExprCtx -> AST.Class -> a -> ExceptState Bool
isAccessible Ctx{cIass=this} target a = case access a of
  Private   -> return $ tthis == ttarget
  Protected ->          tthis <: ttarget
  _         -> return   True
  where tthis   = Instance (name this)
        ttarget = Instance (name target)

checkStatic :: (StaticTag el, PositionTag el) => String -> AccessCtx -> el -> ExceptState ()
checkStatic display ctx el = do
  when (static ctx && not (static el))
    $ throwPretty ("Cannot use non-static " ++ display ++ " in a static context.") $ position el

checkBodyIsBlock :: (BodyTag b AST.Stmt) => b -> ExceptState ()
checkBodyIsBlock b = do
  let _body = body b :: AST.Stmt
  unless (isBlock _body)
    $ throwPretty "The method body must be wrapped in curly parenthesis." $ position _body

checkDuplicateParName :: (ParamsPositionTag p) => p -> ExceptState ()
checkDuplicateParName p = do
  whenJust (listToMaybe $ duplicatesWith (untag :: _ -> String) $ tpnames p)
    $ throwPretty "Duplicate parameter name." . position . last

checkDuplicateSignature :: (NameTag np, ParamsTag np, PositionTag np) => String -> [np] -> ExceptState ()
checkDuplicateSignature object xs = do
  whenJust (listToMaybe $ duplicatesWith signatur xs)
    $ throwPretty ("Duplicate " ++ object ++ ". There already exists a " ++ object ++ " with the same signature.") . position . last

{- type related functions
-}
{- Checks whether the class of an user-written type exists -}
checkTypeValid :: (PositionTag t, Tag t Type) => t -> ExceptState ()
checkTypeValid ttype = case untag ttype of
  (Instance cn) -> void $ resolveClass (cn @ position ttype)
  _             -> return ()

anchestors :: AST.Class -> ExceptState [Identifier]
anchestors cIass = anchestors' (textends cIass) []
  where anchestors' :: Maybe (AST.WithPosition Identifier) -> [Identifier] -> ExceptState [Identifier]
        anchestors' Nothing found = return $ reverse found
        anchestors' (Just tsuperName) found = do
          when (untag tsuperName `elem` found)
            $ throwPretty ("Detected inheritance cycle: " ++ showList found ++ ".") $ position tsuperName
          super <- resolveClass tsuperName
          anchestors' (textends super) (name super : found)

isApplicableTo :: (ParamsTag p) => p -> [Type] -> ExceptState Bool
method `isApplicableTo` targs = matchingLength &&^ matchingTypes
  where matchingLength = return $ length (params method) == length targs
        matchingTypes  = and <$> zipWithM (<:) targs (ptypes method)

-- Return whether parameter types of one method are a specialization of another
(<~:) :: [Type] -> [Type] -> ExceptState Bool
(<~:) tparams1 tparams2 = and <$> zipWithM (<:) tparams1 tparams2

(<:) :: Type -> Type -> ExceptState Bool
Instance cn1 <: Instance cn2 = do
  cIass1 <- flip whenNothingM (throwInternal "Found invalid type. This should be prechecked.")
            $ lookupClass cn1
  return (cn1 == cn2) ||^ (cn2 `elem`) <$> anchestors cIass1
t1 <: t2 = return $ t1 == t2
