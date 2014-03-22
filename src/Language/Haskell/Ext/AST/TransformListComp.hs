{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.TransformListComp

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST


-- | A general /transqual/ in a list comprehension,
--   which could potentially be a transform of the kind
--   enabled by TransformListComp.
data GQualStmt stmt exp id l
    = QualStmt     l stmt         -- ^ an ordinary statement
    | ThenTrans    l exp      -- ^ @then@ /exp/
    | ThenBy       l exp exp  -- ^ @then@ /exp/ @by@ /exp/
    | GroupBy      l exp      -- ^ @then@ @group@ @by@ /exp/
    | GroupUsing   l exp      -- ^ @then@ @group@ @using@ /exp/
    | GroupByUsing l exp exp  -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GQualStmt stmt exp id) where
    ann (QualStmt     l _) = l
    ann (ThenTrans    l _) = l
    ann (ThenBy       l _ _) = l
    ann (GroupBy      l _) = l
    ann (GroupUsing   l _) = l
    ann (GroupByUsing l _ _) = l
    amap = fmap

