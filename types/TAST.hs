{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import           Core

data Type
  = PrimInt
  | PrimBool
  | PrimChar
  | Object TypeName

{- Using "record syntax" for better readability and extensibility
-}
data Class = Class
  { cvisibility :: Visibility
  -- not required
  , cabstract   :: Bool
  , cname       :: ClassName
  -- not required
  , cextends    :: ClassName
  -- not required
  , cimplements :: [InterfaceName]
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
  -- not required
  , mabstract   :: Bool
  , mname       :: FieldName
  , mparams     :: [(Type, LocalName)]
  -- mbody == Nothing <=> mabstract == true, so no checks required if abstract not implemented
  , mbody       :: Maybe Stmt
  }

data Stmt
  = Block Type [Stmt]
  | Return Type Expr
  | While Type Expr Stmt
  | Break Type
  | Continue Type
  | LocalVarDecl Type TypeName LocalName (Maybe Expr)
  | If Type Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt Type StmtOrExpr

data StmtOrExpr
  = Assign Type Expr Expr
  | New Type ClassName [Expr]
  | MethodCall Type Expr [Expr]

data Expr
  = This Type
  | Super Type
  | LocalOrFieldVar Type LocalOrFieldName
  | InstVar Type Expr FieldName
  | Unary Type UnOparator Expr
  | Binary Type BinOperator Expr Expr
  | Integer Type Integer
  | Bool Type Bool
  | Char Type Char
  | String Type String
  | Null Type
  | StmtOrExprAsExpr Type StmtOrExpr
