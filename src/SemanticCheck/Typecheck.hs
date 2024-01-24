{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapAndUnzipM" #-}
module SemanticCheck.Typecheck (typecheck) where

import Control.Composition ((.*))
import Control.Monad.Except
  ( Except,
    MonadError (throwError),
    join,
    runExcept,
    void,
  )
import Control.Monad.Extra (filterM, forM_, when, whenJust)
import Data.Foldable (foldlM)
import Data.Function.Apply ((--$))
import Data.Function.Syntax (slipl)
import Data.List
  ( find,
    intercalate,
    intersperse,
    nub,
    tails,
    uncons,
  )
import Data.List.NonEmpty (unzip)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isJust,
    isNothing,
    mapMaybe,
  )
import Data.Monoid.Extra (First (First, getFirst))
import Data.Tuple.Extra (swap)
import qualified Types.AST as AST
import Types.Core
  ( AccessModifier (..),
    BinOperator (..),
    Identifier,
    Type (..),
    UnOparator (..),
  )
import qualified Types.TAST as TAST
import Prelude hiding (EQ, GT, LT, unzip)

{- General helper functions -}
hasNoDuplicates :: (Eq a) => [a] -> Bool
hasNoDuplicates xs = length xs == length (nub xs)

(<<>) :: Maybe a -> Maybe a -> Maybe a
m1 <<> m2 = getFirst $ First m1 <> First m2

{- General helper functions -}

type Excepting = Except String

liftBool :: e -> Bool -> Except e ()
liftBool _ True = return ()
liftBool err False = throwError err

liftMaybe :: e -> Maybe a -> Except e a
liftMaybe _ (Just x) = return x
liftMaybe err Nothing = throwError err

class Typed a where
  typee :: a -> Type

instance Typed TAST.Expr where
  typee (TAST.This _type) = _type
  typee (TAST.Super _type) = _type
  typee (TAST.LocalVar _type _) = _type
  typee (TAST.ClassRef _type _) = _type
  typee (TAST.FieldAccess _type _ _) = _type
  typee (TAST.Unary _type _ _) = _type
  typee (TAST.Binary _type _ _ _) = _type
  typee (TAST.Literal _type _) = _type
  typee (TAST.StmtOrExprAsExpr _type _) = _type

showSignature :: Identifier -> [Type] -> String
showSignature name tparams = name ++ "(" ++ intercalate ", " (show <$> tparams) ++ ")"

tparams :: AST.Method -> [Type]
tparams = fmap fst . AST.mparams

-- type ThisCtx = Excepting String
-- type StaticCtx = Bool
-- type Ctx = (ThisCtx, StaticCtx)
data ExprCtx = Ctx {locals :: Map Identifier Type, cIass :: AST.Class, static :: Bool, cIasses :: [AST.Class]}

addLocal :: Type -> Identifier -> ExprCtx -> ExprCtx
addLocal tvar name Ctx {locals, cIass, static, cIasses} = Ctx {locals = locals', cIass, static, cIasses}
  where
    locals' = Map.insert name tvar locals

typecheck :: AST.Program -> TAST.Program
typecheck prg = case runExcept $ typecheckM prg of
  Left err -> error err
  Right prg' -> prg'

typecheckM :: AST.Program -> Excepting TAST.Program
typecheckM prg = precheckProgram prg >> checkProgram prg

{- TODO: add missing constructors -}
precheckProgram :: [AST.Class] -> Excepting ()
precheckProgram cIasses = do
  liftBool "Classes must have unique names." $
    hasNoDuplicates $
      AST.cname <$> cIasses
  forM_ cIasses (precheckClass cIasses)

checkProgram :: [AST.Class] -> Excepting TAST.Program
checkProgram classes = do
  mapM (checkClass classes) classes

precheckClass :: [AST.Class] -> AST.Class -> Excepting ()
precheckClass = precheckExtends

checkClass :: [AST.Class] -> AST.Class -> Excepting TAST.Class
checkClass cIasses cIass@AST.Class {AST.caccess, AST.cname, AST.cfields, AST.cmethods} = do
  liftBool ("Accessibility of top level class " ++ cname ++ " must be public or package.") $
    caccess == Public || caccess == Package
  liftBool ("Fields must have unique names in class " ++ cname) $
    hasNoDuplicates $
      AST.fname <$> cfields
  forM_ (safeTails cmethods) $ \(reference, subjects) ->
    forM_ subjects $ checkDuplicateMethod reference
  liftBool ("Methods must have unique names in class " ++ cname) $
    hasNoDuplicates $
      AST.mname <$> cmethods
  tfields <- mapM (checkField cIasses cIass) cfields
  tmethods <- mapM (checkMethod cIasses cIass) cmethods
  return
    TAST.Class
      { TAST.caccess,
        TAST.cname,
        TAST.cextends = [],
        TAST.cfields = tfields,
        TAST.cmethods = tmethods
      }
  where
    safeTails = mapMaybe uncons . tails

checkField :: [AST.Class] -> AST.Class -> AST.Field -> Excepting TAST.Field
checkField cIasses cIass AST.Field {AST.faccess, AST.fstatic, AST.ftype, AST.fname, AST.finit} = do
  finit' <- mapM (checkExpr Ctx {locals = Map.empty, cIass, static = fstatic, cIasses}) finit
  return
    TAST.Field
      { TAST.faccess,
        TAST.ftype,
        TAST.fstatic,
        TAST.fname,
        TAST.finit = finit'
      }

checkMethod :: [AST.Class] -> AST.Class -> AST.Method -> Excepting TAST.Method
checkMethod cIasses cIass method@AST.Method {AST.moverride, AST.maccess, AST.mstatic, AST.mtype, AST.mname, AST.mparams, AST.mbody} = do
  when (mname == AST.cname cIass) $ checkConstructor cIasses cIass method
  liftBool ("Parameters must have unique names in method " ++ AST.cname cIass ++ "." ++ mname ++ ".") $
    hasNoDuplicates $
      snd <$> mparams
  (mbody', defReturn) <- checkStmt Ctx {locals, cIass, static = mstatic, cIasses} mtype mbody
  liftBool ("Not all paths return a value in method " ++ AST.cname cIass ++ "." ++ mname ++ ".") $
    mtype == Void || defReturn
  moverridden <- findOverridden cIasses method (AST.cextends cIass)
  liftBool ("Method " ++ AST.cname cIass ++ "." ++ AST.mname method ++ " is not overriding any method.") $
    moverride && isNothing moverridden
  whenJust moverridden $ \(overCIass, overridden) -> do
    liftBool ("The return type of method " ++ AST.cname cIass ++ "." ++ AST.mname method ++ "  must be a subtype of the overridden method " ++ AST.cname overCIass ++ "." ++ AST.mname overridden ++ ".") $
      (AST.mtype method <: AST.mtype overridden) dummyCtx
    liftBool ("The access modifier of method " ++ AST.cname cIass ++ "." ++ AST.mname method ++ "  must not be stricter than the overridden method " ++ AST.cname overCIass ++ "." ++ AST.mname overridden ++ ".") $
      AST.maccess method <= AST.maccess overridden
  return
    TAST.Method
      { TAST.maccess,
        TAST.mstatic,
        TAST.mtype,
        TAST.mname,
        TAST.mparams,
        TAST.mbody = mbody'
      }
  where
    locals = Map.fromList $ swap <$> mparams
    dummyCtx = Ctx {cIasses, cIass, static = False, locals = Map.empty}
    -- Search for overridden method in superclasses
    findOverridden :: [AST.Class] -> AST.Method -> Maybe Identifier -> Excepting (Maybe (AST.Class, AST.Method))
    findOverridden _ _ Nothing = return Nothing
    findOverridden cIasses method (Just cIassName) = do
      cIass <- _resolveClass cIasses cIassName
      let moverridden = flip find (AST.cmethods cIass) $ \m ->
            AST.mname m == AST.mname method
              && tparams m == tparams method
              && AST.maccess m < Private -- private methods are just not visible
      recursive <- findOverridden cIasses method (AST.cextends cIass)
      return $ ((cIass,) <$> moverridden) <<> recursive

checkConstructor :: [AST.Class] -> AST.Class -> AST.Method -> Excepting ()
checkConstructor _ _ AST.Method {AST.mtype, AST.mname, AST.mbody} = do
  liftBool ("Expected constructor of class " ++ mname ++ " to have return type void.") $
    mtype == Void
  checkReturn mbody
  where
    checkReturn :: AST.Stmt -> Excepting ()
    checkReturn (AST.Block _ stmts) = forM_ stmts checkReturn
    checkReturn (AST.Return pos mexpr) = when (isJust mexpr) $ throwError ("Unexpected return value at " ++ show pos)
    checkReturn (AST.While _ _ stmt) = checkReturn stmt
    checkReturn (AST.LocalVarDecl {}) = return ()
    checkReturn (AST.If _ _ stmt1 mstmt2) = checkReturn stmt1 >> forM_ mstmt2 checkReturn
    checkReturn (AST.StmtOrExprAsStmt _ _) = return ()

checkStmt :: ExprCtx {-target::-} -> Type -> AST.Stmt -> Excepting (TAST.Stmt {-definiteReturn::-}, Bool)
checkStmt _ _ (AST.Block _ []) =
  return (TAST.Block [], False)
checkStmt ctx target (AST.Block blockPos (AST.LocalVarDecl _ tvar name minit : stmts)) = do
  minit' <- mapM (checkExpr ctx) minit
  liftBool ("Expected initializer " ++ show (fromJust minit) ++ " to have declared type " ++ show tvar) $
    slipl maybe minit' True $
      ($ ctx) . (<: tvar) . typee
  (block', defReturns) <- checkStmt (addLocal tvar name ctx) target $ AST.Block blockPos stmts
  -- This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  return (TAST.Block (TAST.LocalVarDecl tvar name minit' : stmts'), defReturns)
checkStmt ctx target (AST.Block blockPos (stmt : stmts)) = do
  (stmt', defReturn) <- checkStmt ctx target stmt
  (block', defReturns) <- checkStmt ctx target $ AST.Block blockPos stmts
  -- This is safe, as AST.Block is used as input
  let (TAST.Block stmts') = block'
  liftBool ("Unreachable code: " ++ show stmts' ++ ".") $
    defReturn && not (null stmts)
  return (TAST.Block (stmt' : stmts'), defReturn || defReturns)
checkStmt ctx target (AST.Return pos mexpr) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  case mexpr' of
    Nothing -> liftBool ("Missing return value at " ++ show pos) $ (Void <: target) ctx
    Just expr' -> liftBool ("Expected " ++ show expr' ++ " to match return type " ++ show target ++ ".") $ (typee expr' <: target) ctx
  return (TAST.Return mexpr', True)
checkStmt ctx target (AST.While _ cond body) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected while condition " ++ show cond' ++ " to have type bool") $
    typee cond' == Bool
  (body', _) <- checkStmt ctx target body
  return (TAST.While cond' body', False)
checkStmt _ _ decl@(AST.LocalVarDecl {}) =
  throwError $ "Variable declaration " ++ show decl ++ " is not allowed here."
checkStmt ctx target (AST.If _ cond ifBranch melseBranch) = do
  cond' <- checkExpr ctx cond
  liftBool ("Expected if condition " ++ show cond' ++ " to have type bool") $
    typee cond' == Bool
  (ifBranch', defReturn1) <- checkStmt ctx target ifBranch
  (melseBranch', mdefReturn2) <- unzip <$> mapM (checkStmt ctx target) melseBranch
  return (TAST.If cond' ifBranch' melseBranch', defReturn1 && fromMaybe False mdefReturn2)
checkStmt ctx _ (AST.StmtOrExprAsStmt pos stmtExpr) = do
  expr' <- checkExpr ctx $ AST.StmtOrExprAsExpr pos stmtExpr
  -- This is safe, as AST.StmtOrExprAsExpr is used as input
  let (TAST.StmtOrExprAsExpr _ stmtExpr') = expr'
  return (TAST.StmtOrExprAsStmt stmtExpr', False)

checkExpr :: ExprCtx -> AST.Expr -> Excepting TAST.Expr
checkExpr Ctx {static, cIass} (AST.This _) = do
  liftBool "this can only be used in a non-static context." $
    not static
  return $ TAST.This (Instance $ AST.cname cIass)
checkExpr _ (AST.Super _) = throwError "super is currently unsupported."
checkExpr ctx@Ctx {static, locals, cIass} (AST.Name pos name) = do
  case (Map.lookup name locals, runExcept $ lookupField ctx name cIass, lookupClass ctx name) of
    (Just t, _, _) -> return $ TAST.LocalVar t name
    (Nothing, Left err, _) -> throwError err
    (Nothing, Right (Just f), _) -> do
      fcheckStatic static f
      let this' = TAST.This (Instance cIassName)
      return $ TAST.FieldAccess (AST.ftype f) this' name
    (Nothing, Right Nothing, Just c) -> return $ TAST.ClassRef (Class $ AST.cname c) (AST.cname c)
    (Nothing, Right Nothing, Nothing) -> throwError $ "Name " ++ name ++ " not found at " ++ show pos
  where
    cIassName = AST.cname cIass
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
    fmap snd $
      flip find types $
        \((op', tin), _) -> op' == op && (typee expr' <: tin) ctx
  return $ TAST.Unary tresult op expr'
  where
    types =
      [ ((Plus, Int), Int),
        ((Minus, Int), Int),
        ((LNot, Bool), Bool)
      ]
checkExpr ctx (AST.Binary _ op expr1 expr2) = do
  expr1' <- checkExpr ctx expr1
  expr2' <- checkExpr ctx expr2
  tresult <- liftMaybe ("Operator " ++ show op ++ " is not defined on type " ++ show (typee expr1') ++ " x " ++ show (typee expr2') ++ ".") $
    fmap snd $
      flip find types $
        \((op', tin1, tin2), _) -> op' == op && (typee expr1' <: tin1) ctx && (typee expr2' <: tin2) ctx
  return $ TAST.Binary tresult op expr1' expr2'
  where
    types =
      [ ((Add, Int, Int), Int),
        ((Sub, Int, Int), Int),
        ((Mul, Int, Int), Int),
        ((Div, Int, Int), Int),
        ((Mod, Int, Int), Int),
        ((LAnd, Bool, Bool), Bool),
        ((LOr, Bool, Bool), Bool),
        ((LT, Int, Int), Bool),
        ((LTE, Int, Int), Bool),
        ((GT, Int, Int), Bool),
        ((GTE, Int, Int), Bool),
        ((EQ, Int, Int), Bool),
        ((EQ, Bool, Bool), Bool),
        ((EQ, Char, Char), Bool),
        ((EQ, Instance "Object", Instance "Object"), Bool),
        ((NEQ, Int, Int), Bool),
        ((NEQ, Bool, Bool), Bool),
        ((NEQ, Char, Char), Bool),
        ((NEQ, Instance "Object", Instance "Object"), Bool)
      ]
checkExpr _ (AST.Literal _ (AST.IntLit i)) = return $ TAST.Literal Int $ TAST.IntLit i
checkExpr _ (AST.Literal _ (AST.CharLit c)) = return $ TAST.Literal Char $ TAST.CharLit c
checkExpr _ (AST.Literal _ (AST.BoolLit b)) = return $ TAST.Literal Bool $ TAST.BoolLit b
checkExpr _ (AST.Literal _ AST.Null) = return $ TAST.Literal NullType TAST.Null
checkExpr ctx@Ctx {locals} (AST.StmtOrExprAsExpr pos (AST.Assign mleft name right)) = do
  mleft' <- mapM (checkExpr ctx) mleft
  right' <- checkExpr ctx right
  tresult <- case (mleft', Map.lookup name locals, runExcept $ lookupField ctx name $ cIass ctx) of
    (Just l', _, _) -> do
      (static, cIassName) <- unpackClassType l'
      f <- resolveField ctx name =<< resolveClass ctx cIassName
      fcheckStatic static f
      return $ AST.ftype f
    (Nothing, Just t, _) -> return t
    (Nothing, Nothing, Left err) -> throwError err
    (Nothing, Nothing, Right (Just f)) -> fcheckStatic (static ctx) f >> return (AST.ftype f)
    (Nothing, Nothing, Right Nothing) -> throwError $ "Name " ++ name ++ " not found."
  liftBool ("Type " ++ show (typee right') ++ " is not assignable to type " ++ show tresult ++ " at " ++ show pos ++ ".") $
    typee right' <: tresult $
      ctx
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.Assign mleft' name right'
checkExpr ctx (AST.StmtOrExprAsExpr _ (AST.New name args)) = do
  args' <- mapM (checkExpr ctx) args
  constructor <- resolveConstructor ctx name (typee <$> args')
  let tresult = Instance name
  let tparams = fst <$> AST.mparams constructor
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.New name (zip tparams args')
checkExpr ctx (AST.StmtOrExprAsExpr _ (AST.MethodCall mexpr name args)) = do
  mexpr' <- mapM (checkExpr ctx) mexpr
  let expr' =
        flip fromMaybe mexpr' $
          if static ctx
            then TAST.ClassRef (Class cIassName) cIassName
            else TAST.This (Instance cIassName)
  args' <- mapM (checkExpr ctx) args
  (tstatic, tname) <- unpackClassType expr'
  (methodCIass, method) <- resolveMethod ctx tname name (typee <$> args')
  mcheckStatic tstatic method
  let tresult = AST.mtype method
  let tparams = fst <$> AST.mparams method
  return $ TAST.StmtOrExprAsExpr tresult $ TAST.MethodCall expr' name (zip tparams args')
  where
    cIassName = AST.cname $ cIass ctx

{- typecheck helper functions
-}
-- doesn't need monad, because all classes are public/package and therefore accessible
_lookupClass :: [AST.Class] -> Identifier -> Maybe AST.Class
_lookupClass cIasses name = find ((name ==) . AST.cname) cIasses

_resolveClass :: [AST.Class] -> Identifier -> Excepting AST.Class
_resolveClass cIasses name = liftMaybe ("Class " ++ name ++ " does not exist.") $ _lookupClass cIasses name

lookupClass :: ExprCtx -> Identifier -> Maybe AST.Class
lookupClass ctx = _lookupClass (cIasses ctx)

resolveClass :: ExprCtx -> Identifier -> Excepting AST.Class
resolveClass ctx = _resolveClass (cIasses ctx)

lookupMethod :: ExprCtx -> Identifier -> Identifier -> [Type] -> Excepting (Maybe (Identifier, AST.Method))
lookupMethod ctx cIassName name targs = do
  cIass <- resolveClass ctx cIassName
  minheritedMethod <- fmap join $ mapM (\cN -> lookupMethod ctx cN name targs) $ AST.cextends cIass
  foldlM (chooseMostSpecific ctx cIassName) minheritedMethod
    =<< filterM
      (misAccessible ctx cIass)
      ( filter (($ ctx) . (`isApplicableTo` targs)) $
          filter (mhasName name) $
            filter (not . mhasName cIassName) $ -- exclude construcors
              AST.cmethods cIass
      )

resolveMethod :: ExprCtx -> Identifier -> Identifier -> [Type] -> Excepting (Identifier, AST.Method)
resolveMethod ctx cIassName name targs =
  liftMaybe ("No method " ++ showSignature name targs ++ " found on class " ++ cIassName ++ ".")
    =<< lookupMethod ctx cIassName name targs

lookupConstructor :: ExprCtx -> Identifier -> [Type] -> Excepting (Maybe AST.Method)
lookupConstructor ctx cIassName targs = do
  cIass <- resolveClass ctx cIassName
  mfound <-
    foldlM (chooseMostSpecific ctx cIassName) Nothing
      =<< filterM
        (misAccessible ctx cIass)
        ( filter (($ ctx) . (`isApplicableTo` targs)) $
            filter (mhasName cIassName) $ -- only constructors
              AST.cmethods cIass
        )
  return $ snd <$> mfound

resolveConstructor :: ExprCtx -> Identifier -> [Type] -> Excepting AST.Method
resolveConstructor ctx cIassName targs =
  liftMaybe ("No constructor " ++ showSignature cIassName targs ++ " found on class " ++ cIassName ++ ".")
    =<< lookupConstructor ctx cIassName targs

chooseMostSpecific :: ExprCtx -> Identifier -> Maybe (Identifier, AST.Method) -> AST.Method -> Excepting (Maybe (Identifier, AST.Method))
chooseMostSpecific ctx cIassName mcNm1 m2 = case mcNm1 of
  Nothing -> return $ Just (cIassName, m2)
  Just (cN1, m1)
    | tparams m1 == tparams m2 -> return $ Just (cIassName, m2) -- equal parameters => m2 is an override => choose m2
    | tparams m1 <~:* tparams m2 -> return $ Just (cN1, m1)
    | tparams m2 <~:* tparams m1 -> return $ Just (cIassName, m2)
    | otherwise -> throwError $ "Reference to " ++ AST.mname m1 ++ " is ambiguous"
  where
    (<~:*) = (<~:) --$ ctx

lookupField :: ExprCtx -> Identifier -> AST.Class -> Excepting (Maybe AST.Field)
lookupField Ctx {cIass = cIassCtx} name cIass = do
  let mfield = find ((name ==) . AST.fname) $ AST.cfields cIass
  whenJust mfield $ \field ->
    liftBool ("Access of field " ++ name ++ " is not permitted from class " ++ AST.cname cIassCtx ++ ".") $
      AST.cname cIassCtx == AST.cname cIass || AST.faccess field <= Package
  return mfield

resolveField :: ExprCtx -> Identifier -> AST.Class -> Excepting AST.Field
resolveField ctx name cIass = liftMaybe ("Field " ++ AST.cname cIass ++ " does in exist on class " ++ name ++ ".") =<< lookupField ctx name cIass

mcheckStatic :: Bool -> AST.Method -> Excepting ()
mcheckStatic static AST.Method {AST.mstatic, AST.mname}
  | static && not mstatic = throwError $ "Cannot use non-static method " ++ mname ++ " in a static context."
  | otherwise = return ()

(<:) t1 t2 _ = t1 == t2
