{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.Ext.AST.TypeFamilies

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST

data GAsstEq ty id l
     = EqualP l (ty id l) (ty id l) -- ^ type equality constraint
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Functor (stmt id) => Annotated (GAsstEq stmt id) where
    ann (EqualP l _ _) = l
    amap = fmap
