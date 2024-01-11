{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.AST where
import           Types.Core

type Program = [Class]

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
  = Block Position [Stmt]
  | Return Position (Maybe Expr)
  | While Position Expr Stmt
  | LocalVarDecl Position Type LocalName (Maybe Expr)
  | If Position Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt Position StmtOrExpr
  deriving (Show, Eq, Ord)

data StmtOrExpr
  = Assign (Maybe Expr) LocalOrFieldName Expr
  | New ClassName [Expr]
  | MethodCall (Maybe Expr) MethodName [Expr]
  deriving (Show, Eq, Ord)

data Expr
  = This Position
  | Super Position
  | Name Position LocalOrFieldOrClassName
  | FieldAccess Position Expr FieldName
  | Unary Position UnOparator Expr
  | Binary Position BinOperator Expr Expr
  | Literal Position Literal
  | StmtOrExprAsExpr Position StmtOrExpr
  deriving (Show, Eq, Ord)
data Literal
  = IntLit Integer
  | CharLit Char
  | BoolLit Bool
  | Null
  deriving (Show, Eq, Ord)

data Position = Position { line :: Integer }
  deriving (Show, Eq, Ord)
