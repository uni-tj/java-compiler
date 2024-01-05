module Typecheck where

import           Control.Monad.Except (Except, liftEither, runExcept,
                                       throwError, withExcept)
import           Data.Map             (Map, fromList, lookup, member, (!))
import           Data.Maybe           (fromJust, isJust)
import           Data.Semigroup       (First (First, getFirst), (<>))
import           Prelude              hiding (EQ, GT, LT, lookup)
import           Types.AST
import qualified Types.Core           as Type (Type (..))
import           Types.Core           (BinOperator (..), Identifier, Type,
                                       UnOparator (..))
import qualified Types.TAST

-- Type class to enable use Expr, Stmt, ...
class Typed a where
  typ :: a -> Type

instance Typed TAST.Expr where
  typ (TAST.This t)               = t
  typ (TAST.Super t)              = t
  typ (TAST.LocalOrFieldVar t _)  = t
  typ (TAST.InstVar t _ _)        = t
  typ (TAST.Unary t _ _)          = t
  typ (TAST.Binary t _ _ _)       = t
  typ (TAST.Integer t _)          = t
  typ (TAST.Bool t _)             = t
  typ (TAST.Char t _)             = t
  typ (TAST.String t _)           = t
  typ (TAST.Null t)               = t
  typ (TAST.StmtOrExprAsExpr t _) = t

-- Map (unary operator, operand type) to return type
unOperatorTypes :: Map (UnOparator, Type) Type
unOperatorTypes = fromList
  [ ((Plus        , Type.PrimInt ), Type.PrimInt )
  , ((Minus       , Type.PrimInt ), Type.PrimInt )
  , ((PreIncrement, Type.PrimInt ), Type.PrimInt )
  , ((PreDecrement, Type.PrimInt ), Type.PrimInt )
  , ((LNot        , Type.PrimBool), Type.PrimBool)
  ]

-- Map (binary operator, left operand type, right operand type) to return type
binOperatorTypes :: Map (BinOperator, Type, Type) Type
binOperatorTypes = fromList
  [ ((Add , Type.PrimInt, Type.PrimInt), Type.PrimInt)
  , ((Add , Type.Object "String", Type.Object "String"), Type.Object "String")
  , ((Sub , Type.PrimInt, Type.PrimInt), Type.PrimInt)
  , ((Mul , Type.PrimInt, Type.PrimInt), Type.PrimInt)
  , ((Div , Type.PrimInt, Type.PrimInt), Type.PrimInt)
  , ((Mod , Type.PrimInt, Type.PrimInt), Type.PrimInt)
  , ((LAnd, Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((LOr , Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((LT  , Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((LTE , Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((GT  , Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((GTE , Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((EQ  , Type.PrimInt, Type.PrimInt), Type.PrimInt)
  , ((EQ  , Type.PrimBool, Type.PrimBool), Type.PrimBool)
  , ((EQ  , Type.PrimChar, Type.PrimChar), Type.PrimChar)
  ]

liftMaybe :: e -> Maybe a -> Except e a
liftMaybe e Nothing  = throwError e
liftMaybe _ (Just x) = return x

liftBool :: e -> Bool -> Except e ()
liftBool e False = throwError e
liftBool _ True  = return ()

instance Semigroup e => Semigroup (Except e a) where
  l <> r = case (runExcept l, runExcept r) of
    (lsucc@(Right _), _) -> liftEither lsucc
    (_, rsucc@(Right _)) -> liftEither rsucc
    (lfail, _)           -> liftEither lfail

isVariable :: TAST.Expr -> Bool
isVariable (TAST.LocalOrFieldVar _ _) = True
isVariable (TAST.InstVar _ _ _)       = True
isVariable _                          = False

-- is t1 a subtype of t2?
(<:) :: Type -> Type -> Bool
Type.AnyObject <: Type.Object _ = True
t1 <: t2                        = t1 == t2

-- upper bound of t1 and t2
(<:>) :: Type -> Type -> Maybe Type
t1 <:> t2
  | t1 <: t2 = Just t2
  | t2 <: t1 = Just t1
  | otherwise = Nothing

data ASTPathEntity
  = PGlobal
  | PClass Identifier
  | PMeth Identifier
  | PVar Identifier
  | PBlock Integer
type ASTPath = [ASTPathEntity]
data Namespace = Namespace
  { vars   :: Map Identifier Variable
  , meths  :: Map Identifier Method
  , cl4sss :: Map Identifier Class }

-- data O =

type Locals = Map Identifier Type
-- Context constists of (local variables, this class if not in static, all classes)
type Context = (Locals, Except String Identifier, Map Identifier Class)

{- deriveVar and deriveMeth together form deriveId
-}
-- deriveLocalVar :: Identifier -> Context -> Except String Type
-- deriveLocalVar id (localVars, _, _) = liftMaybe ("Identifier " ++ id ++ " not found.") $ lookup id localVars
-- deriveVar :: Identifier -> Identifier -> Context -> Except String Type
-- deriveVar cl4ss id variables = liftMaybe ("Identifier " ++ id ++ " not found.") $ lookup id variables

-- deriveMeth :: Identifier ->

deriveExpr :: Expr -> Context -> Except String TAST.Expr
deriveExpr This ctx@(_, mthis, _) = TAST.This . Type.Object <$> mthis
deriveExpr Super _ = throwError "Super is currently not supported due to missing inheritance"
deriveExpr (LocalOrFieldVar id) ctx@(localO, _, _) =
  withExcept (const $ "Identifier " ++ id ++ " not found.")
    $ localVar <> instVar
  where localVar = flip TAST.LocalOrFieldVar id <$> deriveId id localO
        instVar  = deriveExpr (InstVar This id) ctx
deriveExpr (InstVar expr id) ctx@(_, _, classes) = do
  texpr <- deriveExpr expr ctx
  texprO <- case typ texpr of
    Type.Object classId -> return $ classes ! classId
    _                   -> throwError $ show texpr ++ " is not an object type."
  t <- deriveId id texprO
  return $ TAST.InstVar t texpr id
deriveExpr orig@(Unary op expr) ctx = do
  texpr <- deriveExpr expr ctx
  returnType <- liftMaybe "" $ lookup (op, typ texpr) unOperatorTypes
  return $ TAST.Unary returnType op texpr
deriveExpr orig@(Binary op l r) ctx = do
  tl <- deriveExpr l ctx
  tr <- deriveExpr r ctx
  returnType <- liftMaybe "" $ lookup (op, typ tl, typ tr) binOperatorTypes
  return $ TAST.Binary returnType op tl tr
deriveExpr (Integer i) _ = return $ TAST.Integer Type.PrimInt i
deriveExpr (Bool b)    _ = return $ TAST.Bool Type.PrimBool  b
deriveExpr (Char c)    _ = return $ TAST.Char Type.PrimChar c
deriveExpr (String s)  _ = return $ TAST.String (Type.Object "String") s
deriveExpr Null        _ = return $ TAST.Null Type.AnyObject
-- QUESTION: exprl must be LocalVar or InstVar?
deriveExpr (StmtOrExprAsExpr (Assign exprl exprr)) ctx = do
  texprl <- deriveExpr exprl ctx
  texprr <- deriveExpr exprr ctx
  liftBool ("Left hand side of assignment " ++ show texprl ++ " is not a variable.")
    $ isVariable texprl
  liftBool ("Right hand side " ++ show texprr ++ " is not a subtype of " ++ show texprl ++ ".")
    $ typ  texprr <: typ texprl
  return $ TAST.StmtOrExprAsExpr (typ texprl) $ TAST.Assign (typ texprl) texprl texprr
-- TODO: New is fucked up
deriveExpr (StmtOrExprAsExpr (MethodCall expr id args)) ctx = do
  texpr <- deriveExpr expr ctx
  targs <- mapM (flip deriveExpr ctx) args
