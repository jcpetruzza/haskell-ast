{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts, KindSignatures #-}
module Language.Haskell.Ext.AST.Patterns

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding (GPat)


-- | Bang patterns extension to @GPat@
data BangPat pat id l
     = PBangPat l (pat id l) -- ^ strict (bang) pattern: @f !x = ...@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Type signature in pattern (extension to @GPat@)
data PatTySig ty pat id l
     = PatTypeSig l (pat id l) ty         -- ^ pattern with type signature
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- Not really sure what this extension is....
data PatExplTyArg ty (pat :: * -> * -> * -> *) id l
     = PExplTypeArg l (GQName id l) ty     -- ^ Explicit generics style type argument e.g. @f {| Int |} x = ...@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | n+k patterns extension to @GPat@
data NPlusKPat (pat :: * -> * -> *) id l
    = PNPlusK l (GName id l) Integer            -- ^ n+k pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Functor (pat id) => Annotated (BangPat pat id) where
    ann (PBangPat l _) = l
    amap = fmap

instance Functor (pat id) => Annotated (PatTySig ty pat id) where
    ann (PatTypeSig l _ _) = l
    amap = fmap

instance Annotated (PatExplTyArg ty pat id) where
    ann (PExplTypeArg l _ _) = l
    amap = fmap

instance Annotated (NPlusKPat pat id) where
    ann (PNPlusK l _ _) = l
    amap = fmap
