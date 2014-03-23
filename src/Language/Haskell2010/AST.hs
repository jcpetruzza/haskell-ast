{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, StandaloneDeriving #-}
module Language.Haskell2010.AST

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST as Core
import qualified Language.Haskell.AST.Sugar as Sugar
import qualified Language.Haskell.Ext.AST.Patterns as Patterns

-- XXX TODO: Many duplicated types...

-- | No extensions
data NoExts id l

deriving instance Eq (NoExts id l)
deriving instance Ord (NoExts id l)
deriving instance Show (NoExts id l)
deriving instance Functor (NoExts id)
deriving instance Foldable (NoExts id)
deriving instance Traversable (NoExts id)
deriving instance Typeable NoExts
deriving instance (Data id, Data l) => Data (NoExts id l)


-- | This type is used as annotation of @Literals@ in order to
--   store the exact representation
newtype ExactRep s = ExactRep { getExactRep :: s }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

type Literal = Core.GLiteral (ExactRep String)


-- | A Haskell 2010 pattern
type Pat = Core.GPat PatExts
newtype PatExts id l
    = PatSugar (Sugar.GPat Literal Pat id l)
    -- TODO: view patterns
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (PatExts id) where
    ann (PatSugar pat) = ann pat
    amap = fmap

-- | A Haskell 2010 expression
type Exp = Core.GExp Binds Pat Literal ExpExts

newtype ExpExts id l
  = ExpSugar (Sugar.GExp Type QStmt Stmt Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A Haskell 2010 statement
type Stmt = Sugar.GStmt Binds Exp Pat NoExts

-- HACK
type QStmt = ()
type Binds = ()
type Type = ()

