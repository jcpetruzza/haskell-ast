{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.StandaloneDeriving

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( ClassRelatedDecl )


data ClassRelatedDecl asst ty id l
     = DerivDecl    l (Maybe (Context asst id l)) (InstHead ty id l)
     -- ^ A standalone deriving declaration
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (ClassRelatedDecl asst ty id) where
     ann (DerivDecl l _ _) = l
