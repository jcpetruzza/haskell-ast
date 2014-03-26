{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Exts.MultiParamTypeClasses

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core

data GAsstInfix ty id l
     = InfixA l (ty id l) (GQName id l) (ty id l)  -- ^ class assertion where the class name is given infix
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (GAsstInfix ty id) where
    ann (InfixA l _ _ _) = l
