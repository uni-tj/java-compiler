module Core where

type Identifier = String
type ClassName = Identifier
type FieldName = Identifier
type MethodName = Identifier
type LocalName = Identifier
type LocalOrFieldOrClassName = Identifier
data Type
  = Int
  | Char
  | Bool
  | Void
  | Class ClassName
  | NullType
  | StaticClass ClassName
  | StringArr -- to support main method

data Visibility = Public | Private | Package

{- Seperate enums for operators for better type safety
-}
data UnOparator
  = Plus
  | Minus
  | PreIncrement -- prefix
  | PreDecrement -- prefix
  | LNot
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
