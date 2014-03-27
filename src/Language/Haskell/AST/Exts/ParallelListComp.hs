{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.ParallelListComp

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding (Decl, Exp)

-- | Extension of @Exp@ with parallel list comprehensions
data Exp qstmt exp id l
     = ParComp  l (exp  id l) [[qstmt id l]]     -- ^ parallel list comprehension
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Exp qstmt exp id) where
    ann (ParComp l _ _) = l
