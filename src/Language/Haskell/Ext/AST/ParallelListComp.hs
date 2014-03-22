{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.ParallelListComp

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding (GDecl, GExp)

-- | Extension of @GExp@ with parallel list comprehensions
data GExp qstmt exp l
     = ParComp  l exp [[qstmt]]     -- ^ parallel list comprehension
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GExp exp id) where
    ann (ParComp l _ _) = l
    amap = fmap