{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.AST where
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
  = This
  | Super
  | Name LocalOrFieldOrClassName
  | FieldAccess Expr FieldName
  | Unary UnOparator Expr
  | Binary BinOperator Expr Expr
  | Literal Literal
  | StmtOrExprAsExpr StmtOrExpr
  deriving (Show, Eq, Ord)
data Literal
  = IntLit Integer
  | CharLit Char
  | BoolLit Bool
  | Null
  deriving (Show, Eq, Ord)

type Program = [Class]
