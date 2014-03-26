{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.ViewPatterns

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( GPat )

-- | Extension of the @GPat@ with view patterns
data GPat exp pat id l
    = PViewPat l exp pat            -- ^ view patterns of the form @(/exp/ -> /pat/)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GPat exp pat id) where
   ann (PViewPat l _ _) = l
