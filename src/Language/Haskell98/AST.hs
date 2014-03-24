{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, StandaloneDeriving #-}
module Language.Haskell98.AST

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST as Core
import qualified Language.Haskell.AST.Sugar as Sugar
import qualified Language.Haskell.Ext.AST.Patterns as Patterns

-- | No extensions
data NoExts id l

deriving instance Eq (NoExts id l)
deriving instance Ord (NoExts id l)
deriving instance Show (NoExts id l)
deriving instance Functor (NoExts id)
deriving instance Foldable (NoExts id)
deriving instance Traversable (NoExts id)
deriving instance Typeable NoExts
deriving instance (Data id, Data l) => Data (NoExts id l)

instance Annotated (NoExts id) where
    ann  = error "ann / Annotated NoExts"
    amap = error "fmap / Annotated NoExts"

-- | This type is used as annotation of @Literals@ in order to
--   store the exact representation
newtype ExactRep s = ExactRep { getExactRep :: s }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

type Literal = Core.GLiteral (ExactRep String)

-- | A Haskell 98 pattern
type Pat = Core.GPat PatExts

data PatExts id l
    = PatSugar   (Sugar.GPat Literal Pat id l)
    | PatSugar98 (Patterns.NPlusKPat Pat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (PatExts id) where
    ann (PatSugar   pat) = ann pat
    ann (PatSugar98 pat) = ann pat
    amap = fmap

-- | A Haskell 98 expression
type Exp = Core.GExp Binds Pat Literal ExpExts

newtype ExpExts id l
  = ExpSugar (Sugar.GExp Binds Type Guard Pat StmtExts Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- Only expressions can be used as guards in H98...
type Guard = Exp

instance Annotated (ExpExts id) where
    ann (ExpSugar exp) = ann exp
    amap = fmap

-- | A Haskell 98 statement
type Stmt = Sugar.GStmt Binds Exp Pat StmtExts

type StmtExts = NoExts

-- | A Haskell 98 assertion is of the form "C a", with a variable
--   (should be, e.g., a type without context, for FlexibleContexts...)
type Assertion = Core.GAsst GName AssertionExts
type AssertionExts = NoExts

-- | A Haskell 98 type
type Type = Core.GType Assertion TypeExts

newtype TypeExts id l
  = TypeSugar (Sugar.GType Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (TypeExts id) where
    ann (TypeSugar t) = ann t
    amap = fmap

-- HACK
type Binds = ()



-- | Haskell 98 @let@ or @where@ declarations
-- XXXX TODO!!


