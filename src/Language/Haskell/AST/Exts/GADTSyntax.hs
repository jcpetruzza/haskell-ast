{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.GADTSyntax

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( TypeDecl )

data TypeDecl asst ty id l
    = GDataDecl   l (DataOrNew l) (Maybe (Context asst id l)) (DeclHead id l) (Maybe (Kind id l)) [GadtDecl ty id l]    (Maybe (Deriving ty id l))
     -- ^ A data OR newtype declaration, GADT style
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A single constructor declaration in a GADT data type declaration.
data GadtDecl ty id l
    = GadtDecl l (Name id l) (ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (TypeDecl asst ty id) where
  ann (GDataDecl l _ _ _ _ _ _) = l

instance Annotated (GadtDecl ty id) where
    ann (GadtDecl l _ _) = l

