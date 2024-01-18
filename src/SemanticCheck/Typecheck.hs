{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}
module SemanticCheck.Typecheck(checkProgram) where

import           Control.Monad.Except (Except, MonadError (throwError),
                                       runExcept)
import           Control.Monad.Extra  (forM_, when, whenJust)
import           Data.Foldable        (foldlM)
import           Data.List            (find, intersperse, nub, tails, uncons)
import           Data.List.NonEmpty   (unzip)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Tuple.Extra     (swap)
import           Prelude              hiding (EQ, GT, LT, unzip)
import qualified Types.AST            as AST
import           Types.Core           (AccessModifier (..), BinOperator (..),
                                       Identifier, Type (..), UnOparator (..))
import qualified Types.TAST           as TAST

{- General helper functions -}
hasNoDuplicates :: Eq a => [a] -> Bool
hasNoDuplicates xs = length xs == length (nub xs)

infixr 8 -.
(-.) :: (a -> b) -> (b -> c) -> a -> c
(-.) = flip (.)
{- General helper functions -}

type Excepting = Except String
liftBool :: e -> Bool -> Except e ()
liftBool _   True  = return ()
liftBool err False = throwError err
liftMaybe :: e -> Maybe a -> Except e a
liftMaybe _ (Just x)  = return x
liftMaybe err Nothing = throwError err

class Typed a where
  typee :: a -> Type
instance Typed TAST.Expr where
  typee (TAST.This _type)               = _type
  typee (TAST.Super _type)              = _type
  typee (TAST.Name _type _)             = _type
  typee (TAST.FieldAccess _type _ _)    = _type
  typee (TAST.Unary _type _ _)          = _type
  typee (TAST.Binary _type _ _ _)       = _type
  typee (TAST.Literal _type _)          = _type
  typee (TAST.StmtOrExprAsExpr _type _) = _type

-- type ThisCtx = Excepting String
-- type StaticCtx = Bool
-- type Ctx = (ThisCtx, StaticCtx)
data ExprCtx = Ctx { locals :: Map Identifier Type, cIass :: AST.Class, static :: Bool, cIasses :: [AST.Class] }
addLocal :: Type -> Identifier -> ExprCtx -> ExprCtx
addLocal tvar name Ctx{locals,cIass,static,cIasses} = Ctx{locals=locals', cIass, static, cIasses}
  where locals' = Map.insert name tvar locals

{- TODO: add missing constructors -}
checkProgram :: [AST.Class] -> Excepting TAST.Program
checkProgram classes = do
  liftBool "Classes must have unique names." $
    hasNoDuplicates $ AST.cname <$> classes
  mapM (checkClass classes) classes

checkClass :: [AST.Class] -> AST.Class -> Excepting TAST.Class
checkClass cIasses cIass@AST.Class{AST.caccess, AST.cname, AST.cfields, AST.cmethods} = do
  liftBool ("Accessibility of top level class " ++ cname ++ " must be public or package.") $
    caccess == Public || caccess == Package
  liftBool ("Fields must have unique names in class " ++ cname) $
    hasNoDuplicates $ AST.fname <$> cfields
  forM_ (safeTails cmethods) $ \(reference,subjects) ->
    forM_ subjects $ checkDuplicateMethod reference
  liftBool ("Methods must have unique names in class " ++ cname) $
    hasNoDuplicates $ AST.mname <$> cmethods
  tfields <- mapM (checkField cIasses cIass) cfields
  tmethods <- mapM (checkMethod cIasses cIass) cmethods
  return TAST.Class {
    TAST.caccess,
    TAST.cname,
    TAST.cextends = [],
    TAST.cfields = tfields,
    TAST.cmethods = tmethods
  }
  where safeTails = mapMaybe uncons . tails

checkField :: [AST.Class] -> AST.Class -> AST.Field -> Excepting TAST.Field
checkField cIasses cIass AST.Field { AST.faccess, AST.fstatic, AST.ftype, AST.fname, AST.finit } = do
  finit' <- mapM (checkExpr Ctx{locals = Map.empty, cIass, static = fstatic, cIasses}) finit
  return TAST.Field {
    TAST.faccess,
    TAST.ftype,
    TAST.fstatic,
    TAST.fname,
    TAST.finit = finit'
  }

checkMethod :: [AST.Class] -> AST.Class -> AST.Method -> Excepting TAST.Method
checkMethod cIasses cIass method@AST.Method{ AST.maccess, AST.mstatic, AST.mtype, AST.mname, AST.mparams, AST.mbody } = do
  when (mname == AST.cname cIass) $ checkConstructor cIasses cIass method
  liftBool ("Parameters must have unique names in method " ++ AST.cname cIass ++ "." ++ mname ++ ".") $
    hasNoDuplicates $ snd <$> mparams
  (mbody', defReturn) <- checkStmt Ctx{locals,cIass,static=mstatic,cIasses} mtype mbody
  liftBool ("Not all paths return a value in method " ++ AST.cname cIass ++ "." ++ mname ++ ".") $
    mtype == Void || defReturn
  return TAST.Method {
    TAST.maccess,
    TAST.mstatic,
    TAST.mtype,
    TAST.mname,
    TAST.mparams,
    TAST.mbody = mbody'
  }
  where locals = Map.fromList $ swap <$> mparams


checkConstructor :: [AST.Class] -> AST.Class -> AST.Method -> Excepting ()
checkConstructor _ _ AST.Method{ AST.mtype, AST.mname, AST.mbody } = do
  liftBool ("Expected constructor of class " ++ mname ++ " to have return type void.") $
    mtype == Void
  checkReturn mbody
  where checkReturn :: AST.Stmt -> Excepting ()
        checkReturn (AST.Block _ stmts) = forM_ stmts checkReturn
        checkReturn (AST.Return pos mexpr) = when (isJust mexpr) $ throwError ("Unexpected return value at " ++ show pos)
        checkReturn (AST.While _ _ stmt) = checkReturn stmt
        checkReturn (AST.LocalVarDecl {}) = return ()
        checkReturn (AST.If _ _ stmt1 mstmt2) = checkReturn stmt1 >> forM_ mstmt2 checkReturn
        checkReturn (AST.StmtOrExprAsStmt _ _) = return ()


checkStmt :: ExprCtx -> {-target::-}Type -> AST.Stmt -> Excepting (TAST.Stmt, {-definiteReturn::-}Bool)
checkStmt _   _      (AST.Block _ []) =
  return (TAST.Block [], False)
checkStmt ctx target (AST.Block blockPos (AST.LocalVarDecl _ tvar name minit:stmts)) = do
  minit' <- mapM (checkExpr ctx) minit
  liftBool ("Expected initializer " ++ show (fromJust minit) ++ " to have declared type " ++ show tvar) $
    maybe True (typee -. (<: tvar)) minit'
  (block', defReturns) <- checkStmt (addLocal tvar name ctx) target $ AST.Block blockPos stmts
   -- This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  return (TAST.Block (TAST.LocalVarDecl tvar name minit':stmts'), defReturns)
checkStmt ctx target (AST.Block blockPos (stmt:stmts)) = do
  (stmt', defReturn) <- checkStmt ctx target stmt
  (block', defReturns) <- checkStmt ctx target $ AST.Block blockPos stmts
   -- This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  liftBool ("Unreachable code: " ++ show stmts' ++ ".") $
    defReturn && not (null stmts)
  return (TAST.Block (stmt':stmts'), defReturn || defReturns)
checkStmt ctx target (AST.Return pos mexpr) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  case mexpr' of
    Nothing -> liftBool ("Missing return value at " ++ show pos) $ Void <: target
    Just expr' -> liftBool ("Expected " ++ show expr' ++ " to match return type " ++ show target ++ ".") $ typee expr' <: target
  return (TAST.Return mexpr', True)
checkStmt ctx target (AST.While _ cond body) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected while condition " ++ show cond' ++ " to have type bool") $
    typee cond' == Bool
  (body', _) <- checkStmt ctx target body
  return (TAST.While cond' body', False)
checkStmt _   _      decl@(AST.LocalVarDecl {}) =
  throwError $ "Variable declaration " ++ show decl ++ " is not allowed here."
checkStmt ctx target (AST.If _ cond ifBranch melseBranch) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected if condition " ++ show cond' ++ " to have type bool") $
    typee cond' == Bool
  (ifBranch', defReturn1) <- checkStmt ctx target ifBranch
  (melseBranch', mdefReturn2) <- unzip <$> mapM (checkStmt ctx target) melseBranch
  return (TAST.If cond' ifBranch' melseBranch', defReturn1 && fromMaybe False mdefReturn2)
checkStmt ctx _      (AST.StmtOrExprAsStmt pos stmtExpr) = do
  expr' <- checkExpr ctx $ AST.StmtOrExprAsExpr pos stmtExpr
   -- This is safe, as AST.StmtOrExprAsExpr is used as input
  let (TAST.StmtOrExprAsExpr _ stmtExpr') = expr'
  return (TAST.StmtOrExprAsStmt stmtExpr', False)

checkExpr :: ExprCtx -> AST.Expr -> Excepting TAST.Expr
checkExpr Ctx{static, cIass} (AST.This _) = do
  liftBool "this can only be used in a non-static context." $
    not static
  return $ TAST.This (Instance $ AST.cname cIass)
checkExpr _ (AST.Super _) = throwError "super is currently unsupported."
checkExpr ctx@Ctx{static, locals, cIass} (AST.Name pos name) = do
  tresult <- case (Map.lookup name locals, runExcept $ lookupField ctx name cIass, lookupClass ctx name) of
    (Just t , _             , _      ) -> return t
    (Nothing, Left err      , _      ) -> throwError err
    (Nothing, Right (Just f), _      ) -> fcheckStatic static f >> return (AST.ftype f)
    (Nothing, Right Nothing , Just c ) -> return $ Class (AST.cname c)
    (Nothing, Right Nothing , Nothing) -> throwError $ "Name " ++ name ++ " not found at " ++ show pos
  return $ TAST.Name tresult name
checkExpr ctx (AST.FieldAccess _ expr name) = do
  expr' <- checkExpr ctx expr
  (tstatic, tname) <- unpackClassType expr'
  cIass <- resolveClass ctx tname
  field <- resolveField ctx name cIass
  fcheckStatic tstatic field
  return $ TAST.FieldAccess (AST.ftype field) expr' name
checkExpr ctx (AST.Unary _ op expr) = do
  expr' <- checkExpr ctx expr
  tresult <- liftMaybe ("Operator " ++ show op ++ " is not defined on type " ++ show (typee expr') ++ ".") $
    fmap snd $ flip find types $ \((op', tin),_) -> op' == op && typee expr' <: tin
  return $ TAST.Unary tresult op expr'
    where types =
            [ ((Plus        , Int ), Int )
            , ((Minus       , Int ), Int )
            , ((PreIncrement, Int ), Int )
            , ((PreDecrement, Int ), Int )
            , ((LNot        , Bool), Bool)
            ]
checkExpr ctx (AST.Binary _ op expr1 expr2) = do
  expr1' <- checkExpr ctx expr1
  expr2' <- checkExpr ctx expr2
  tresult <- liftMaybe ("Operator " ++ show op ++ " is not defined on type " ++ show (typee expr1') ++ " x " ++ show (typee expr2') ++ ".") $
    fmap snd $ flip find types $ \((op', tin1, tin2),_) -> op' == op && typee expr1' <: tin1 && typee expr2' <: tin2
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
checkExpr ctx@Ctx{locals} (AST.StmtOrExprAsExpr _ (AST.Assign mleft name right)) = do
  mleft' <- mapM (checkExpr ctx) mleft
  right' <- checkExpr ctx right
  tresult <- case (mleft', Map.lookup name locals, runExcept $ lookupField ctx name $ cIass ctx) of
    (Just l', _      , _             ) -> do
                                          (static, cIassName) <- unpackClassType l'
                                          f <- resolveField ctx name =<< resolveClass ctx cIassName
                                          fcheckStatic static f
                                          return $ AST.ftype f
    (Nothing, Just t , _             ) -> return t
    (Nothing, Nothing, Left err      ) -> throwError err
    (Nothing, Nothing, Right (Just f)) -> fcheckStatic (static ctx) f >> return (AST.ftype f)
    (Nothing, Nothing, Right Nothing ) -> throwError $ "Name " ++ name ++ " not found."
  liftBool (show right' ++ " is not assignable to type " ++ show tresult ++ ".") $
    typee right' <: tresult
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.Assign mleft' name right'
checkExpr ctx (AST.StmtOrExprAsExpr _ (AST.New name args)) = do
  args' <- mapM (checkExpr ctx) args
  params <- AST.mparams <$> (resolveMethod ctx args' name =<< resolveClass ctx name)
  checkArgs args' params
  let tresult = Instance name
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.New name args'
checkExpr ctx (AST.StmtOrExprAsExpr _ (AST.MethodCall mexpr name args)) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  args' <- mapM (checkExpr ctx) args
  (tstatic, tname) <- case mexpr' of
              Just expr' -> unpackClassType expr'
              Nothing    -> return (static ctx, AST.cname $ cIass ctx)
  method <- resolveMethod ctx args' name =<< resolveClass ctx tname
  mcheckStatic tstatic method
  checkArgs args' $ AST.mparams method
  let tresult = AST.mtype method
  let tparams = fst <$> AST.mparams method
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.MethodCall mexpr' name (zip tparams args')

{- typecheck helper functions
-}
-- doesn't need monad, because all classes are public/package and therefore accessible
lookupClass :: ExprCtx -> Identifier -> Maybe AST.Class
lookupClass ctx name = find ((name ==) . AST.cname) $ cIasses ctx
resolveClass :: ExprCtx -> Identifier -> Excepting AST.Class
resolveClass ctx name = liftMaybe ("Class " ++ name ++ " does not exist.") $ lookupClass ctx name

lookupMethod :: ExprCtx -> [TAST.Expr] -> Identifier -> AST.Class -> Excepting (Maybe AST.Method)
lookupMethod Ctx{cIass=cIassCtx} args' name cIass = do
  -- let mmethod = find ((name ==) . AST.mname) $ AST.cmethods cIass
  mmethod <- foldlM chooseOverload Nothing $ filter ((name ==) . AST.mname) $ AST.cmethods cIass
  whenJust mmethod $ \method ->
    liftBool ("Access of method " ++ name ++ " is not permitted from class " ++ AST.cname cIassCtx ++ ".")
    $ AST.cname cIassCtx == AST.cname cIass || AST.maccess method <= Package
  return mmethod
  where chooseOverload :: Maybe AST.Method -> AST.Method -> Excepting (Maybe AST.Method)
        chooseOverload mm1 m2 = case (mm1, m2 `isApplicableTo` args') of
          (Nothing, False) -> return Nothing
          (Nothing, True ) -> return $ Just m2
          (Just m1, False) -> return $ Just m1
          (Just m1, True )
            -- Equal parameter types fall in the first case. They are illegal though and throw an error when the corresponding class is checked
            | tparams m1 <~: tparams m2 -> return $ Just m1
            | tparams m2 <~: tparams m1 -> return $ Just m2
            | otherwise                 -> throwError $ "Reference to " ++ AST.mname m1 ++ " is ambiguous"
        tparams = map fst . AST.mparams
resolveMethod :: ExprCtx -> [TAST.Expr] -> Identifier -> AST.Class -> Excepting AST.Method
resolveMethod ctx args' name cIass = liftMaybe ("Method " ++ AST.cname cIass ++ " does in exist on class " ++ name ++ ".") =<< lookupMethod ctx args' name cIass

lookupField :: ExprCtx -> Identifier -> AST.Class -> Excepting (Maybe AST.Field)
lookupField Ctx{cIass=cIassCtx} name cIass = do
  let mfield = find ((name ==) . AST.fname) $ AST.cfields cIass
  whenJust mfield $ \field ->
    liftBool ("Access of field " ++ name ++ " is not permitted from class " ++ AST.cname cIassCtx ++ ".")
    $ AST.cname cIassCtx == AST.cname cIass || AST.faccess field <= Package
  return mfield
resolveField :: ExprCtx -> Identifier -> AST.Class -> Excepting AST.Field
resolveField ctx name cIass = liftMaybe ("Field " ++ AST.cname cIass ++ " does in exist on class " ++ name ++ ".") =<< lookupField ctx name cIass

mcheckStatic :: Bool -> AST.Method -> Excepting ()
mcheckStatic static AST.Method{AST.mstatic, AST.mname}
  | static && not mstatic = throwError $ "Cannot use non-static method " ++ mname ++ " in a static context."
  | otherwise             = return ()
fcheckStatic :: Bool -> AST.Field -> Excepting ()
fcheckStatic static AST.Field{AST.fstatic, AST.fname}
  | static && not fstatic = throwError $ "Cannot use non-static method " ++ fname ++ " in a static context."
  | otherwise             = return ()

checkArgs :: [TAST.Expr] -> [(Type, Identifier)] -> Excepting ()
checkArgs args' params = do
  liftBool ("Expected " ++ show (length params) ++ " arguments, but got " ++ show (length args') ++ ".") $
    length params == length args'
  sequence_ $ flip fmap (zip args' params) $ \(arg', (tparam,paramName)) ->
    liftBool ("Expected argument " ++ show arg' ++ " of parameter " ++ paramName ++ " to have type " ++ show tparam ++ ".")
    $ typee arg' <: tparam

checkDuplicateMethod :: AST.Method -> AST.Method -> Excepting ()
checkDuplicateMethod m1 m2 = if AST.mname m1 /= AST.mname m2 then return () else do
  liftBool ("Method " ++ AST.mname m1 ++ "(" ++ intersperse ',' (show $ fst <$> AST.mparams m1) ++ ") is already defined.")
  $  (fst <$> AST.mparams m1) == (fst <$> AST.mparams m2)

{- type related functions
-}

unpackClassType :: TAST.Expr -> Excepting (Bool, Identifier)
unpackClassType expr' = case typee expr' of
  (Instance       name) -> return (False, name)
  (Class name) -> return (True , name)
  _                  -> throwError $ "Expected " ++ show expr' ++ "to have class type."

isApplicableTo :: AST.Method -> [TAST.Expr] -> Bool
AST.Method{AST.mparams} `isApplicableTo` args'
  =  length mparams == length args'
  && and (zipWith (<:) targs tparams)
  where tparams = fst <$> mparams
        targs   = typee <$> args'

-- Return whether parameter types of one method are a specialization of another
(<~:) :: [Type] -> [Type] -> Bool
tparams1 <~: tparams2 = and $ zipWith (<:) tparams1 tparams2

(<:) :: Type -> Type -> Bool
(Instance _) <: (Instance "Object") = True
t1        <: t2                     = t1 == t2
