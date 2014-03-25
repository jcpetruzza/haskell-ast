{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.Ext.AST.StandaloneDeriving

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GClassRelatedDecl )


data GClassRelatedDecl asst ty id l
     = DerivDecl    l (Maybe (GContext asst id l)) (GInstHead ty id l)
     -- ^ A standalone deriving declaration
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GClassRelatedDecl asst ty id) where
     ann (DerivDecl l _ _) = l
