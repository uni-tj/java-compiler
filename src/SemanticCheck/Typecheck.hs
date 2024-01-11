{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}
module Typecheck(checkProgram) where

import           Control.Monad        (when)
import           Control.Monad.Except (Except, MonadError (throwError))
import           Data.List            (find, nub)
import           Data.List.NonEmpty   (unzip)
import           Data.Map
import           Data.Maybe           (fromJust, fromMaybe)
import           Prelude              hiding (EQ, GT, LT, unzip)
import qualified Types.AST            as AST
import           Types.Core           (BinOperator (..), Identifier, Type (..),
                                       UnOparator (..), Visibility (..))
import qualified Types.TAST           as TAST

{- General helper functions -}
hasNoDuplicates :: Eq a => [a] -> Bool
hasNoDuplicates xs = length xs == length (nub xs)

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

infixr 8 -.
(-.) :: (a -> b) -> (b -> c) -> a -> c
(-.) = flip (.)
{- General helper functions -}

type Excepting = Except String
liftBool :: e -> Bool -> Except e ()
liftBool _   True  = return ()
liftBool err False = throwError err
liftBoolM :: e -> Except e Bool -> Except e ()
liftBoolM e mbool = mbool >>= liftBool e
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
  where locals' = insert name tvar locals

{- TODO: add missing constructors -}
checkProgram :: [AST.Class] -> Excepting TAST.Program
checkProgram classes = do
  liftBool "Classes must have unique names." $
    hasNoDuplicates $ AST.cname <$> classes
  mapM (checkClass classes) classes

checkClass :: [AST.Class] -> AST.Class -> Excepting TAST.Class
checkClass cIasses cIass@AST.Class{AST.cvisibility, AST.cname, AST.cfields, AST.cmethods} = do
  liftBool "Visibility of top level class must be public or package." $
    cvisibility == Public || cvisibility == Package
  liftBool ("Fields must have unique names in class " ++ cname) $
    hasNoDuplicates $ AST.fname <$> cfields
  liftBool ("Methods must have unique names in class " ++ cname) $
    hasNoDuplicates $ AST.mname <$> cmethods
  tfields <- mapM (checkField cIasses cIass) cfields
  tmethods <- mapM (checkMethod cIasses cIass) cmethods
  return TAST.Class {
    TAST.cvisibility,
    TAST.cname,
    TAST.cextends = [],
    TAST.cfields = tfields,
    TAST.cmethods = tmethods
  }

checkField :: [AST.Class] -> AST.Class -> AST.Field -> Excepting TAST.Field
checkField cIasses cIass AST.Field { AST.ftype, AST.fstatic, AST.fname, AST.finit } = do
  finit' <- mapM (checkExpr Ctx{locals = empty, cIass, static = fstatic, cIasses}) finit
  return TAST.Field {
    TAST.ftype,
    TAST.fstatic,
    TAST.fname,
    TAST.finit = finit'
  }

checkMethod :: [AST.Class] -> AST.Class -> AST.Method -> Excepting TAST.Method
checkMethod cIasses cIass method@AST.Method{ AST.mvisibility, AST.mtype, AST.mstatic, AST.mname, AST.mparams, AST.mbody } = do
  when (mname == AST.cname cIass) $ checkConstructor cIasses cIass method
  liftBool ("Parameters must have unique names in method " ++ AST.cname cIass ++ "." ++ mname ++ ".") $
    hasNoDuplicates $ snd <$> mparams
  (mbody', defReturn) <- checkStmt Ctx{locals=empty,cIass,static=mstatic,cIasses} mtype mbody
  liftBool ("Not all paths return a value in method " ++ AST.cname cIass ++ "." ++ mname ++ ".") $
    mtype == Void || defReturn
  return TAST.Method {
    TAST.mvisibility,
    TAST.mtype,
    TAST.mstatic,
    TAST.mname,
    TAST.mparams,
    TAST.mbody = mbody'
  }


checkConstructor :: [AST.Class] -> AST.Class -> AST.Method -> Excepting ()
checkConstructor _ _ AST.Method{ AST.mtype, AST.mname, AST.mbody } = do
  liftBool ("Expected constructor of class " ++ mname ++ " to have return type void.") $
    mtype == Void
  liftBool ("Unexpected return value in constructor of class " ++ mname ++ ".") $
    hasReturn mbody
  where hasReturn (AST.Block stmts) = any hasReturn stmts
        hasReturn (AST.Return _) = True
        hasReturn (AST.While _ stmt) = hasReturn stmt
        hasReturn (AST.LocalVarDecl {}) = False
        hasReturn (AST.If _ stmt1 mstmt2) = hasReturn stmt1 || maybe False hasReturn mstmt2
        hasReturn (AST.StmtOrExprAsStmt _) = False


checkStmt :: ExprCtx -> {-target::-}Type -> AST.Stmt -> Excepting (TAST.Stmt, {-definiteReturn::-}Bool)
checkStmt _   _      (AST.Block []) =
  return (TAST.Block [], False)
checkStmt ctx target (AST.Block (AST.LocalVarDecl tvar name minit:stmts)) = do
  minit' <- mapM (checkExpr ctx) minit
  liftBool ("Expected initializer " ++ show (fromJust minit) ++ " to have declared type " ++ show tvar) $
    maybe True (typee -. (<: tvar)) minit'
  (block', defReturns) <- checkStmt (addLocal tvar name ctx) target $ AST.Block stmts
   -- This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  return (TAST.Block (TAST.LocalVarDecl tvar name minit':stmts'), defReturns)
checkStmt ctx target (AST.Block (stmt:stmts)) = do
  (stmt', defReturn) <- checkStmt ctx target stmt
  (block', defReturns) <- checkStmt ctx target $ AST.Block stmts
   -- This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  liftBool ("Unreachable code: " ++ show stmts' ++ ".") $
    defReturn && not (Prelude.null stmts)
  return (TAST.Block (stmt':stmts'), defReturn || defReturns)
checkStmt ctx target (AST.Return expr) = do
  expr' <- checkExpr ctx expr
  liftBool ("Expected " ++ show expr' ++ " to match return type " ++ show target ++ ".") $
    typee expr' <: target
  return (TAST.Return expr', True)
checkStmt ctx target (AST.While cond body) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected while condition " ++ show cond' ++ " to have type bool") $
    typee cond' == Bool
  (body', _) <- checkStmt ctx target body
  return (TAST.While cond' body', False)
checkStmt _   _      decl@(AST.LocalVarDecl {}) =
  throwError $ "Variable declaration " ++ show decl ++ " is not allowed here."
checkStmt ctx target (AST.If cond ifBranch melseBranch) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected if condition " ++ show cond' ++ " to have type bool") $
    typee cond' == Bool
  (ifBranch', defReturn1) <- checkStmt ctx target ifBranch
  (melseBranch', mdefReturn2) <- unzip <$> mapM (checkStmt ctx target) melseBranch
  return (TAST.If cond' ifBranch' melseBranch', defReturn1 && fromMaybe False mdefReturn2)
checkStmt ctx _      (AST.StmtOrExprAsStmt stmtExpr) = do
  expr' <- checkExpr ctx $ AST.StmtOrExprAsExpr stmtExpr
   -- This is safe, as AST.StmtOrExprAsExpr is used as input
  let (TAST.StmtOrExprAsExpr _ stmtExpr') = expr'
  return (TAST.StmtOrExprAsStmt stmtExpr', False)

checkExpr :: ExprCtx -> AST.Expr -> Excepting TAST.Expr
checkExpr Ctx{static, cIass} AST.This = do
  liftBool "this can only be used in a non-static context." $
    not static
  return $ TAST.This (Instance $ AST.cname cIass)
checkExpr ctx@Ctx{static, locals, cIass} (AST.Name name) = do
  tresult <- case (Data.Map.lookup name locals, resolveField name cIass, resolveClass ctx name) of
    (Just t , _      , _      ) -> return t
    (Nothing, Just f , _      ) -> fcheckStatic static f >> return (AST.ftype f)
    (Nothing, Nothing, Just c ) -> return $ Class (AST.cname c)
    (Nothing, Nothing, Nothing) -> throwError $ "Name " ++ name ++ " not found."
  return $ TAST.Name tresult name
checkExpr ctx (AST.FieldAccess expr name) = do
  expr' <- checkExpr ctx expr
  (tstatic, tname) <- unpackClassType expr'
  cIass <- resolveClassM ctx tname
  field <- resolveFieldM name cIass
  fcheckStatic tstatic field
  return $ TAST.FieldAccess (AST.ftype field) expr' name
checkExpr ctx (AST.Unary op expr) = do
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
checkExpr ctx (AST.Binary op expr1 expr2) = do
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
checkExpr _ (AST.Literal (AST.IntLit i)) = return $ TAST.Literal Int $ TAST.IntLit i
checkExpr _ (AST.Literal (AST.CharLit c)) = return $ TAST.Literal Char $ TAST.CharLit c
checkExpr _ (AST.Literal (AST.BoolLit b)) = return $ TAST.Literal Bool $ TAST.BoolLit b
checkExpr _ (AST.Literal (AST.Null)) = return $ TAST.Literal NullType TAST.Null
checkExpr ctx@Ctx{locals} (AST.StmtOrExprAsExpr (AST.Assign mleft name right)) = do
  mleft' <- mapM (checkExpr ctx) mleft
  right' <- checkExpr ctx right
  tresult <- case (mleft', Data.Map.lookup name locals, resolveField name $ cIass ctx) of
    (Just l', _      , _      ) -> do
                                    (static, cIassName) <- unpackClassType l'
                                    f <- resolveFieldM name =<< resolveClassM ctx cIassName
                                    fcheckStatic static f
                                    return $ AST.ftype f
    (Nothing, Just t , _      ) -> return t
    (Nothing, Nothing, Just f ) -> fcheckStatic (static ctx) f >> return (AST.ftype f)
    (Nothing, Nothing, Nothing) -> throwError $ "Name " ++ name ++ " not found."
  liftBool (show right' ++ " is not assignable to type " ++ show tresult ++ ".") $
    typee right' <: tresult
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.Assign mleft' name right'
checkExpr ctx (AST.StmtOrExprAsExpr (AST.New name args)) = do
  params <- AST.mparams <$> (resolveMethodM name =<< resolveClassM ctx name)
  args' <- mapM (checkExpr ctx) args
  checkArgs args' params
  let tresult = Instance name
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.New name args'
checkExpr ctx (AST.StmtOrExprAsExpr (AST.MethodCall mexpr name args)) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  args' <- mapM (checkExpr ctx) args
  (tstatic, tname) <- case mexpr' of
              Just expr' -> unpackClassType expr'
              Nothing    -> return (static ctx, AST.cname $ cIass ctx)
  method <- resolveMethodM name =<< resolveClassM ctx tname
  mcheckStatic tstatic method
  checkArgs args' $ AST.mparams method
  let tresult = AST.mtype method
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.MethodCall mexpr' name args'

{- typecheck helper functions
-}
resolveClass :: ExprCtx -> Identifier -> Maybe AST.Class
resolveClass ctx name = find ((name ==) . AST.cname) $ cIasses ctx
resolveClassM :: ExprCtx -> Identifier -> Excepting AST.Class
resolveClassM ctx name = liftMaybe ("Class " ++ name ++ " does not exist.") $ resolveClass ctx name

resolveMethod :: Identifier -> AST.Class -> Maybe AST.Method
resolveMethod name cIass = find ((name ==) . AST.mname) $ AST.cmethods cIass
resolveMethodM :: Identifier -> AST.Class -> Excepting AST.Method
resolveMethodM name cIass = liftMaybe ("Method " ++ AST.cname cIass ++ " does in exist on class " ++ name ++ ".") $ resolveMethod name cIass

resolveField :: Identifier -> AST.Class -> Maybe AST.Field
resolveField name cIass = find ((name ==) . AST.fname) $ AST.cfields cIass
resolveFieldM :: Identifier -> AST.Class -> Excepting AST.Field
resolveFieldM name cIass = liftMaybe ("Field " ++ AST.cname cIass ++ " does in exist on class " ++ name ++ ".") $ resolveField name cIass

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

{- type related functions
-}

unpackClassType :: TAST.Expr -> Excepting (Bool, Identifier)
unpackClassType expr' = case typee expr' of
  (Instance       name) -> return (False, name)
  (Class name) -> return (True , name)
  _                  -> throwError $ "Expected " ++ show expr' ++ "to have class type."

(<:) :: Type -> Type -> Bool
(Instance _) <: (Instance "Object") = True
t1        <: t2                     = t1 == t2
