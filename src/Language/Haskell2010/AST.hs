{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell2010.AST

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST as Core
import qualified Language.Haskell.AST.Sugar as Sugar
import qualified Language.Haskell.Ext.AST.Patterns as Patterns

-- XXX TODO: Many duplicated types...

-- | End of extensions (empty declaration)
data EOE

-- | This type is used as annotation of @Literals@ in order to
--   store the exact representation
newtype ExactRep s = ExactRep { getExactRep :: s }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

type Literal = Core.GLiteral (ExactRep String)


-- | A Haskell 2010 pattern
type Pat = Core.GPat PatExts
newtype PatExts id l
    = PatSugar (Sugar.GPat Literal Pat id l)
    -- TODO: view patterns
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (PatExts id) where
    ann (PatSugar pat) = ann pat
    amap = fmap