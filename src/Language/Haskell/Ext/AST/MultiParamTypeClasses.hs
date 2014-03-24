{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.Ext.AST.MultiParamTypeClasses

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST

data GAsstInfix ty id l
     = InfixA l (ty id l) (GQName id l) (ty id l)  -- ^ class assertion where the class name is given infix
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Functor (ty id) => Annotated (GAsstInfix ty id) where
    ann (InfixA l _ _ _) = l
    amap = fmap
