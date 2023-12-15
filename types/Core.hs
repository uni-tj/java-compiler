module Core where

type Identifier = String
type ClassName = Identifier
type InterfaceName = Identifier
type FieldName = Identifier
type LocalName = Identifier
type LocalOrFieldName = Identifier
type TypeName = Identifier

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
