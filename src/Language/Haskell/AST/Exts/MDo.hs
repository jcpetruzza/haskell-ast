{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.MDo

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Stmt )

-- | Extension of @GStmt@ with mdo
data Stmt stmt l
    = MDo l [stmt] -- ^ @mdo@-expression
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Stmt stmt) where
    ann (MDo l _) = l
