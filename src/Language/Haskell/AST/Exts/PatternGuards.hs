{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.PatternGuards

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( GuardedAlt )

-- | A guard according to the PatternGuard extension
data PatternGuard stmt id l
    = PatternGuard l [stmt id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (PatternGuard stmt id) where
    ann (PatternGuard l _) = l

