module Types.Core where

type Identifier = String
type ClassName = Identifier
type FieldName = Identifier
type MethodName = Identifier
type LocalName = Identifier
type LocalOrFieldName = Identifier
type LocalOrFieldOrClassName = Identifier
{- DISCUSSION: rename Class to ClassInstance and StaticClass to ClassTyoe -}
data Type
  = Int
  | Char
  | Bool
  | Void
  | NullType
  | Instance ClassName
  | Class ClassName
  | StringArr -- to support main method
  deriving (Eq, Ord)
instance Show Type where
  show Int           = "int"
  show Char          = "char"
  show Bool          = "boolean"
  show Void          = "void"
  show NullType      = "null"
  show (Instance cn) = cn
  show (Class cn)    = cn ++ "[Class]"
  show StringArr     = "String[]"


data AccessModifier = Public | Package | Protected | Private
  deriving (Show, Eq, Ord)

{- Seperate enums for operators for better type safety
-}
data UnOparator
  = Plus
  | Minus
  | LNot
  deriving (Show, Eq, Ord)
data BinOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | LAnd
  | LOr
  | LT
  | LTE
  | GT
  | GTE
  | EQ
  | NEQ
  deriving (Show, Eq, Ord)
