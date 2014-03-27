{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.ViewPatterns

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Pat )

-- | Extension of the @Pat@ with view patterns
data Pat exp pat id l
    = PViewPat l (exp id l) (pat id l)           -- ^ view patterns of the form @(/exp/ -> /pat/)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Pat exp pat id) where
   ann (PViewPat l _ _) = l
