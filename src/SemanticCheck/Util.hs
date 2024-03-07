{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
module SemanticCheck.Util (typee', TypeTag(..), AccessTag(..), StaticTag(..), NameTag(..), ParamsTag(..), ExtendsTag(..), BodyTag(..), TypePositionTag(..), AccessPositionTag(..), StaticPositionTag(..), NamePositionTag(..), ParamsPositionTag(..), ExtendsPositionTag(..), PositionTag(..), Tag(..), From(..), FromPartial(..), simpleName, Signature, signatur, showSignature, showList, isBlock, traverseStmtM) where

import           Control.Monad.Except (MonadError, forM_)
import           Control.Monad.Extra  (whenJust)
import           Data.Bifunctor       (bimap)
import           Data.Functor.Syntax  ((<$$>))
import           Data.List.Extra      (intercalate, splitOn)
import           Prelude              hiding (showList)
import qualified Types.AST            as AST
import           Types.Core           (AccessModifier (..), Identifier,
                                       Type (..))
import qualified Types.TAST           as TAST

-- Extracts the computed type of an expression
typee' :: TAST.Expr -> Type
typee' (TAST.This _type)                                          = _type
typee' (TAST.Super _type)                                         = _type
typee' (TAST.LocalVar _type _)                                    = _type
typee' (TAST.ClassRef _type _)                                    = _type
typee' (TAST.FieldAccess _type _ _ _ _)                           = _type
typee' (TAST.Unary _type _ _)                                     = _type
typee' (TAST.Binary _type _ _ _)                                  = _type
typee' (TAST.Literal _type _)                                     = _type
typee' (TAST.StmtOrExprAsExpr (TAST.LocalAssign _type _ _))       = _type
typee' (TAST.StmtOrExprAsExpr (TAST.FieldAssign _type _ _ _ _ _)) = _type
typee' (TAST.StmtOrExprAsExpr (TAST.New _type _ _))               = _type
typee' (TAST.StmtOrExprAsExpr (TAST.MethodCall _type _ _ _ _ _))  = _type

class TypeTag a where
  typee :: a -> Type
instance TypePositionTag a => TypeTag a where
  typee = untag . ttypee
instance TypeTag TAST.Method where typee = TAST.mtype
instance TypeTag TAST.Field  where typee = TAST.ftype
class TypePositionTag a where
  ttypee :: a -> AST.WithPosition Type
instance TypePositionTag AST.Method where ttypee = AST.mtype
instance TypePositionTag AST.Field  where ttypee = AST.ftype

class AccessTag a where
  access :: a -> AccessModifier
instance AccessPositionTag a => AccessTag a where
  access = untag . taccess
instance AccessTag TAST.Method      where access = TAST.maccess
instance AccessTag TAST.Constructor where access = TAST.craccess
instance AccessTag TAST.Field       where access = TAST.faccess
instance AccessTag TAST.Class       where access = TAST.caccess
class AccessPositionTag a where
  taccess :: a -> AST.WithPosition AccessModifier
instance AccessPositionTag AST.Method      where taccess = AST.maccess
instance AccessPositionTag AST.Constructor where taccess = AST.craccess
instance AccessPositionTag AST.Field       where taccess = AST.faccess
instance AccessPositionTag AST.Class       where taccess = AST.caccess

class StaticTag a where
  static :: a -> Bool
instance StaticPositionTag a => StaticTag a where
  static = untag . tstatic
instance StaticTag TAST.Method where static = TAST.mstatic
instance StaticTag TAST.Field  where static = TAST.fstatic
class StaticPositionTag a where
  tstatic :: a -> AST.WithPosition Bool
instance StaticPositionTag AST.Method where tstatic = AST.mstatic
instance StaticPositionTag AST.Field  where tstatic = AST.fstatic

class NameTag a where
  name :: a -> String
instance NamePositionTag a => NameTag a where
  name = untag . tname
instance NameTag TAST.Method      where name = TAST.mname
instance NameTag TAST.Field       where name = TAST.fname
instance NameTag TAST.Class       where name = TAST.cname
class NamePositionTag a where
  tname :: a -> AST.WithPosition String
instance NamePositionTag AST.Method      where tname = AST.mname
instance NamePositionTag AST.Constructor where tname = AST.crname
instance NamePositionTag AST.Field       where tname = AST.fname
instance NamePositionTag AST.Class       where tname = AST.cname

class ParamsTag a where
  params :: a -> [(Type, Identifier)]
  ptypes :: a -> [Type]
  ptypes = fmap fst . params
  pnames :: a -> [Identifier]
  pnames = fmap snd . params
instance ParamsPositionTag a => ParamsTag a where
  params = bimap untag untag <$$> tparams
instance ParamsTag TAST.Method      where params = TAST.mparams
instance ParamsTag TAST.Constructor where params = TAST.crparams
class ParamsPositionTag a where
  tparams :: a -> [(AST.WithPosition Type, AST.WithPosition Identifier)]
  tptypes :: a -> [AST.WithPosition Type]
  tptypes = fmap fst . tparams
  tpnames :: a -> [AST.WithPosition Identifier]
  tpnames = fmap snd . tparams
instance ParamsPositionTag AST.Method      where tparams = AST.mparams
instance ParamsPositionTag AST.Constructor where tparams = AST.crparams

class ExtendsTag a where
  extends :: a -> Maybe Identifier
instance ExtendsPositionTag a => ExtendsTag a where
  extends = untag <$$> textends
class ExtendsPositionTag a where
  textends :: a -> Maybe (AST.WithPosition Identifier)
instance ExtendsPositionTag AST.Class where textends = AST.cextends

class BodyTag a b where
  body :: a -> b
instance BodyTag AST.Method      AST.Stmt where body = AST.mbody
instance BodyTag AST.Constructor AST.Stmt where body = AST.crbody
instance BodyTag TAST.Method      TAST.Stmt where body = TAST.mbody
instance BodyTag TAST.Constructor TAST.Stmt where body = TAST.crbody

class PositionTag a where
  position :: a -> AST.Position
instance PositionTag (AST.WithPosition a) where
  position (AST.WithPosition pos _) = pos
instance PositionTag AST.Class       where position = AST.cposition
instance PositionTag AST.Field       where position = AST.fposition
instance PositionTag AST.Method      where position = AST.mposition
instance PositionTag AST.Constructor where position = AST.crposition
instance PositionTag AST.Expr where
  position (AST.This pos)               = pos
  position (AST.Super pos)              = pos
  position (AST.Name pos _)             = pos
  position (AST.FieldAccess pos _ _)    = pos
  position (AST.Unary pos _ _)          = pos
  position (AST.Binary pos _ _ _)       = pos
  position (AST.Literal pos _)          = pos
  position (AST.StmtOrExprAsExpr pos _) = pos
instance PositionTag AST.Stmt where
  position (AST.Block pos _)            = pos
  position (AST.Return pos _)           = pos
  position (AST.While pos _ _)          = pos
  position (AST.LocalVarDecl pos _ _ _) = pos
  position (AST.If pos _ _ _)           = pos
  position (AST.ThisCall pos _)         = pos
  position (AST.SuperCall pos _)        = pos
  position (AST.StmtOrExprAsStmt pos _) = pos

class Tag t a | t -> a where
  untag :: t -> a
instance Tag (AST.WithPosition a) a where
  untag (AST.WithPosition _ v) = v

class From b a where from :: a -> b

class FromPartial b a e where fromM :: MonadError e m => a -> m b

simpleName :: AST.Class -> Identifier
simpleName = last . splitOn "/" . name

{- The signature of a method -}
type Signature = (Identifier, [Type])
signatur :: (NameTag np, ParamsTag np) => np -> Signature
signatur np = (name np, ptypes np)

{- Prettyprint method signature -}
showSignature :: Identifier -> [Type] -> String
showSignature fname partypes = fname ++ "(" ++ intercalate ", " (show <$> partypes) ++ ")"

{- Print comma-separated list -}
showList :: Show a => [a] -> String
showList = intercalate ", " . map show

isBlock :: AST.Stmt -> Bool
isBlock AST.Block{} = True
isBlock _           = False

{- traverse a stmt recursively -}
traverseStmtM :: Monad m => (AST.Stmt -> m ()) -> AST.Stmt -> m ()
traverseStmtM f stmt = f stmt >> case stmt of
  (AST.Block _ substmts)          -> forM_ substmts (traverseStmtM f)
  (AST.While _ _ substmt)         -> traverseStmtM f substmt
  (AST.If _ _ substmt1 msubstmt2) -> traverseStmtM f substmt1 >> whenJust msubstmt2 (traverseStmtM f)
  _                               -> return ()
