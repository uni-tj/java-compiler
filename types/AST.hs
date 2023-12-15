{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import           Core

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
  { ftype :: TypeName
  , fname :: FieldName
  , finit :: Maybe Expr
  }
data Method = Method
  { mvisibility :: Visibility
  , mtype       :: TypeName
  , mstatic     :: Bool
  -- not required
  , mabstract   :: Bool
  , mname       :: FieldName
  , mparams     :: [(TypeName, LocalName)]
  -- mbody == Nothing <=> mabstract == true, so no checks required if abstract not implemented
  , mbody       :: Maybe Stmt
  }

data Stmt
  = Block [Stmt]
  | Return Expr
  | While Expr Stmt
  | LocalVarDecl TypeName LocalName (Maybe Expr)
  | If Expr Stmt (Maybe Stmt)
  | StmtOrExprAsStmt StmtOrExpr

data StmtOrExpr
  = Assign Expr Expr
  | New ClassName [Expr]
  | MethodCall Expr [Expr]

data Expr
  = This
  | Super
  | LocalOrFieldVar LocalOrFieldName
  | InstVar Expr FieldName
  | Unary UnOparator Expr
  | Binary BinOperator Expr Expr
  | Integer Integer
  | Bool Bool
  | Char Char
  | String String
  | Null
  | StmtOrExprAsExpr StmtOrExpr

type Program = [Class]
