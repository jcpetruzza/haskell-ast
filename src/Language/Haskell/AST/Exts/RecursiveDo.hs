{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.RecursiveDo

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Exp )

-- | Extension of @Exp@ with mdo
data Exp stmt id l
    = MDo l [stmt id l] -- ^ @mdo@-expression
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Exp stmt id) where
    ann (MDo l _) = l
