{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.AST where
import           Types.Core

type Program = [Class]

{- Using "record syntax" for better readability and extensibility
-}
data Class = Class
  { caccess       :: AccessModifier
  , cname         :: ClassName
  -- not required
  , cextends      :: Maybe ClassName
  , cfields       :: [Field]
  , cmethods      :: [Method]
  , cconstructors :: [Constructor]
  }
  deriving (Show, Eq, Ord)

data Field = Field
  { foverride :: Bool
  , faccess   :: AccessModifier
  , fstatic   :: Bool
  , ftype     :: Type
  , fname     :: FieldName
  , finit     :: Maybe Expr
  }
  deriving (Show, Eq, Ord)
data Method = Method
  { moverride :: Bool
  , maccess   :: AccessModifier
  , mstatic   :: Bool
  , mtype     :: Type
  , mname     :: FieldName
  , mparams   :: [(Type, LocalName)]
  , mbody     :: Stmt
  }
  deriving (Show, Eq, Ord)
data Constructor = Constructor
  { craccess :: AccessModifier
  , crname   :: ClassName -- only for convenience
  , crparams :: [(Type, LocalName)]
  , crbody   :: Stmt
  }
  deriving (Show, Eq, Ord)

data Stmt
  = Block Position [Stmt]
  | Return Position (Maybe Expr)
  | While Position Expr Stmt
  | LocalVarDecl Position Type LocalName (Maybe Expr)
  | If Position Expr Stmt (Maybe Stmt)
  | ThisCall Position [Expr]  -- can only appear as first stmt of constructor
  | SuperCall Position [Expr] -- can only appear as first stmt of constructor
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

data Position = Position { start :: (Integer, Integer), end :: (Integer, Integer) }
  deriving (Show, Eq, Ord)
