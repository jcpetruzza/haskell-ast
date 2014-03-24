{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.MDo

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GStmt )

-- | Extension of @GStmt@ with mdo
data GStmt stmt l
    = MDo l [stmt] -- ^ @mdo@-expression
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GStmt stmt) where
    ann (MDo l _) = l
    amap = fmap