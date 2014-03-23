{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.Ext.AST.PatternGuards

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GGuardedAlt )

-- | A guard according to the PatternGuard extension
data PatternGuard stmt id l
    = PatternGuard l [stmt id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Functor (stmt id) => Annotated (PatternGuard stmt id) where
    ann (PatternGuard l _) = l
    amap = fmap

