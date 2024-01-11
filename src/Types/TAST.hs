{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.TAST where
import           Types.Core

{- Using "record syntax" for better readability and extensibility
-}
data Class = Class
  { cvisibility :: Visibility
  , cname       :: ClassName
  -- not required
  , cextends    :: ClassName
  , cfields     :: [Field]
  , cmethods    :: [Method]
  }
  deriving (Show, Eq, Ord)

data Field = Field
  { ftype   :: Type
  , fstatic :: Bool
  , fname   :: FieldName
  , finit   :: Maybe Expr
  }
  deriving (Show, Eq, Ord)
data Method = Method
  { mvisibility :: Visibility
  , mtype       :: Type
  , mstatic     :: Bool
  , mname       :: FieldName
  , mparams     :: [(Type, LocalName)]
  , mbody       :: Stmt
  }
  deriving (Show, Eq, Ord)

{- DISCUSSION: remove types from statements or how to interpret type of statements? -}
data Stmt
  = Block [Stmt]
  | Return Expr
  | While Expr Stmt
  | LocalVarDecl Type LocalName (Maybe Expr)
  | If Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt StmtOrExpr
  deriving (Show, Eq, Ord)

data StmtOrExpr
  = Assign (Maybe Expr) LocalOrFieldName Expr
  | New ClassName [Expr]
  | MethodCall (Maybe Expr) MethodName [Expr]
  deriving (Show, Eq, Ord)

data Expr
  = This Type
  | Super Type
  | Name Type LocalOrFieldOrClassName
  | FieldAccess Type Expr FieldName
  | Unary Type UnOparator Expr
  | Binary Type BinOperator Expr Expr
  | Literal Type Literal
  | StmtOrExprAsExpr Type StmtOrExpr
  deriving (Show, Eq, Ord)
data Literal
  = IntLit Integer
  | CharLit Char
  | BoolLit Bool
  | Null
  deriving (Show, Eq, Ord)

type Program = [Class]
