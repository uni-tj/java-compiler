{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.TAST where
import           Types.Core

type Program = [Class]

{- Using "record syntax" for better readability and extensibility
-}
data Class = Class
  { caccess  :: AccessModifier
  , cname    :: ClassName
  -- not required
  , cextends :: ClassName
  , cfields  :: [Field]
  , cmethods :: [Method]
  }
  deriving (Show, Eq, Ord)

data Field = Field
  { faccess :: AccessModifier
  , fstatic :: Bool
  , ftype   :: Type
  , fname   :: FieldName
  , finit   :: Maybe Expr
  }
  deriving (Show, Eq, Ord)
data Method = Method
  { maccess :: AccessModifier
  , mtype   :: Type
  , mstatic :: Bool
  , mname   :: FieldName
  , mparams :: [(Type, LocalName)]
  , mbody   :: Stmt
  }
  deriving (Show, Eq, Ord)

{- DISCUSSION: remove types from statements or how to interpret type of statements? -}
data Stmt
  = Block [Stmt]
  | Return (Maybe Expr)
  | While Expr Stmt
  | LocalVarDecl Type LocalName (Maybe Expr)
  | If Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt StmtOrExpr
  deriving (Show, Eq, Ord)

data StmtOrExpr
  = Assign (Maybe Expr) LocalOrFieldName Expr
  | New ClassName [Expr]
  | MethodCall (Maybe Expr) MethodName [(Type, Expr)]
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
