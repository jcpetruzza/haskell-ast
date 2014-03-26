{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Exts.TransformListComp

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( GExp )

-- | Extension of the @GExp@ type with extended list comprehensions
data GExp stmt exp id l
     = EListComp l (exp id l) [GQualStmt stmt exp id l]  -- ^ an extended list comprehension
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A general /transqual/ in a list comprehension,
--   which could potentially be a transform of the kind
--   enabled by TransformListComp.
data GQualStmt stmt exp id l
    = QualStmt     l (stmt id l)            -- ^ an ordinary statement
    | ThenTrans    l (exp id l)             -- ^ @then@ /exp/
    | ThenBy       l (exp id l) (exp id l)  -- ^ @then@ /exp/ @by@ /exp/
    | GroupBy      l (exp id l)             -- ^ @then@ @group@ @by@ /exp/
    | GroupUsing   l (exp id l)             -- ^ @then@ @group@ @using@ /exp/
    | GroupByUsing l (exp id l) (exp id l)  -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GExp stmt exp id) where
    ann (EListComp l _ _) = l

instance Annotated (GQualStmt stmt exp id) where
    ann (QualStmt     l _)   = l
    ann (ThenTrans    l _)   = l
    ann (ThenBy       l _ _) = l
    ann (GroupBy      l _)   = l
    ann (GroupUsing   l _)   = l
    ann (GroupByUsing l _ _) = l

