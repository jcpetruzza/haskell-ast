{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell98.AST

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST as Core
import qualified Language.Haskell.AST.Sugar as Sugar
import qualified Language.Haskell.Ext.AST.Patterns as Patterns

-- | End of extensions (empty declaration)
data EOE

-- | This type is used as annotation of @Literals@ in order to
--   store the exact representation
newtype ExactRep s = ExactRep { getExactRep :: s }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

type Literal = Core.GLiteral (ExactRep String)

-- | A Haskell 98 pattern
type Pat = Core.GPat PatExts

data PatExts id l
    = PatSugar   (Sugar.GPat Literal Pat id l)
    | PatSugar98 (Patterns.NPlusKPat Pat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (PatExts id) where
    ann (PatSugar   pat) = ann pat
    ann (PatSugar98 pat) = ann pat
    amap = fmap

-- | A Haskell 98 expression
type Exp = Core.GExp Binds Pat Literal ExpExts

newtype ExpExts id l
  = ExpSugar (Sugar.GExp Type QStmt Stmt Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- HACK
type QStmt = ()
type Stmt = ()
type Binds = ()
type Type = ()

instance Annotated (ExpExts id) where
    ann (ExpSugar exp) = ann exp
    amap = fmap


-- | Haskell 98 @let@ or @where@ declarations
-- XXXX TODO!!


