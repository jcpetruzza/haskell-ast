{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.PatternGuards

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GGuardedAlt )

-- | A guarded case alternative @|@ /stmt/ @->@ /exp/.
-- | NB. This follows the haskell'2010 specification (with pattern guards)
data GGuardedAlt stmt binds pat lit id l
    = PGuardedAlt l [stmt] (GExp binds pat lit id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GGuardedAlt stmt binds pat lit id) where
    ann (PGuardedAlt l _ _) = l
    amap = fmap

