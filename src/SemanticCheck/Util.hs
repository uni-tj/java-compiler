{-# LANGUAGE MultiParamTypeClasses #-}
module SemanticCheck.Util (TypeTag(..), AccessTag(..), StaticTag(..), NameTag(..), ParamsTag(..), From(..), FromPartial(..)) where

import           Control.Monad.Except (MonadError)
import qualified Types.AST            as AST
import           Types.Core           (AccessModifier (..), Identifier,
                                       Type (..))
import qualified Types.TAST           as TAST

class TypeTag a where
  typee :: a -> Type
instance TypeTag TAST.Expr where
  typee (TAST.This _type)                                          = _type
  typee (TAST.Super _type)                                         = _type
  typee (TAST.LocalVar _type _)                                    = _type
  typee (TAST.ClassRef _type _)                                    = _type
  typee (TAST.FieldAccess _type _ _ _ _)                           = _type
  typee (TAST.Unary _type _ _)                                     = _type
  typee (TAST.Binary _type _ _ _)                                  = _type
  typee (TAST.Literal _type _)                                     = _type
  typee (TAST.StmtOrExprAsExpr (TAST.LocalAssign _type _ _))       = _type
  typee (TAST.StmtOrExprAsExpr (TAST.FieldAssign _type _ _ _ _ _)) = _type
  typee (TAST.StmtOrExprAsExpr (TAST.New _type _ _))               = _type
  typee (TAST.StmtOrExprAsExpr (TAST.MethodCall _type _ _ _ _ _))  = _type
instance TypeTag AST.Method where typee = AST.mtype
instance TypeTag AST.Field  where typee = AST.ftype
instance TypeTag TAST.Method where typee = TAST.mtype
instance TypeTag TAST.Field  where typee = TAST.ftype

class AccessTag a where
  access :: a -> AccessModifier
instance AccessTag AST.Method      where access = AST.maccess
instance AccessTag AST.Constructor where access = AST.craccess
instance AccessTag AST.Field       where access = AST.faccess
instance AccessTag AST.Class       where access = AST.caccess
instance AccessTag TAST.Method      where access = TAST.maccess
instance AccessTag TAST.Constructor where access = TAST.craccess
instance AccessTag TAST.Field       where access = TAST.faccess
instance AccessTag TAST.Class       where access = TAST.caccess

class StaticTag a where
  static :: a -> Bool
instance StaticTag AST.Method where static = AST.mstatic
instance StaticTag AST.Field  where static = AST.fstatic
instance StaticTag TAST.Method where static = TAST.mstatic
instance StaticTag TAST.Field  where static = TAST.fstatic

class NameTag a where
  name :: a -> String
instance NameTag AST.Method      where name = AST.mname
instance NameTag AST.Constructor where name = AST.crname
instance NameTag AST.Field       where name = AST.fname
instance NameTag AST.Class       where name = AST.cname
instance NameTag TAST.Method      where name = TAST.mname
instance NameTag TAST.Field       where name = TAST.fname
instance NameTag TAST.Class       where name = TAST.cname

class ParamsTag a where
  params :: a -> [(Type, Identifier)]
  ptypes :: a -> [Type]
  ptypes = fmap fst . params
  pnames :: a -> [Identifier]
  pnames = fmap snd . params
instance ParamsTag AST.Method      where params = AST.mparams
instance ParamsTag AST.Constructor where params = AST.crparams
instance ParamsTag TAST.Method      where params = TAST.mparams
instance ParamsTag TAST.Constructor where params = TAST.crparams

class From b a where from :: a -> b

class FromPartial b a e where fromM :: MonadError e m => a -> m b
