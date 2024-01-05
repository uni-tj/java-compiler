{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module AST where
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
  = Block [Stmt]
  | Return Expr
  | While Expr Stmt
  | LocalVarDecl Type LocalName (Maybe Expr)
  | If Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt StmtOrExpr

data StmtOrExpr
  = Assign Expr Expr
  | New ClassName [Expr]
  | MethodCall Expr MethodName [Expr]

data Expr
  = This
  | Super
  | Name LocalOrFieldOrClassName
  | FieldAccess Expr FieldName
  | Unary UnOparator Expr
  | Binary BinOperator Expr Expr
  | Literal Literal
  | StmtOrExprAsExpr StmtOrExpr
data Literal
  = IntLit Integer
  | CharLit Char
  | BoolLit Bool
  | Null

type Program = [Class]
