{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module TAST where
import           Core

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

data Field = Field
  { ftype :: Type
  , fname :: FieldName
  , finit :: Maybe Expr
  }
data Method = Method
  { mvisibility :: Visibility
  , mtype       :: Type
  , mstatic     :: Bool
  , mname       :: FieldName
  , mparams     :: [(Type, LocalName)]
  , mbody       :: Stmt
  }

data Stmt
  = Block Type [Stmt]
  | Return Type Expr
  | While Type Expr Stmt
  | LocalVarDecl Type Type LocalName (Maybe Expr)
  | If Type Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt Type StmtOrExpr

data StmtOrExpr
  = Assign Type Expr Expr
  | New Type ClassName [Expr]
  | MethodCall Type Expr MethodName [Expr]

data Expr
  = This Type
  | Super Type
  | Name Type LocalOrFieldOrClassName
  | FieldAccess Type Expr FieldName
  | Unary Type UnOparator Expr
  | Binary Type BinOperator Expr Expr
  | Literal Type Literal
  | StmtOrExprAsExpr Type StmtOrExpr
data Literal
  = IntLit Type Integer
  | CharLit Type Char
  | BoolLit Type Bool
  | Null Type

