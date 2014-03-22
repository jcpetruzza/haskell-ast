{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.HaRP

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST


-- | HaRP extensions to the @GPat@ type: regular list pattern
data GPat_RegList stmt pat id l
    = PRPat l [GRPat stmt pat id l]       -- ^ regular list pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | HaRP extensions to the @GPat@ type: regular list pattern
data GPat_XRegList stmt pat id l
    =  PXRPats  l [GRPat stmt pat id l]   -- ^ XML regular list pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A regular pattern operator.
data GRPatOp l
    = RPStar  l  -- ^ @*@ = 0 or more
    | RPStarG l  -- ^ @*!@ = 0 or more, greedy
    | RPPlus  l  -- ^ @+@ = 1 or more
    | RPPlusG l  -- ^ @+!@ = 1 or more, greedy
    | RPOpt   l  -- ^ @?@ = 0 or 1
    | RPOptG  l  -- ^ @?!@ = 0 or 1, greedy
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An entity in a regular pattern.
data GRPat stmt pat id l
    = RPOp l (GRPat stmt pat id l) (GRPatOp l)   -- ^ operator pattern, e.g. pat*
    | RPEither l (GRPat stmt pat id l) (GRPat stmt pat id l) -- ^ choice pattern, e.g. (1 | 2)
    | RPSeq l [GRPat stmt pat id l]             -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
    | RPGuard l pat [stmt]   -- ^ guarded pattern, e.g. (| p | p < 3 |)
    | RPCAs l (GName id l) (GRPat stmt pat id l)    -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
    | RPAs l (GName id l) (GRPat stmt pat id l)     -- ^ linear variable binding, e.g. foo\@(1 | 2)
    | RPParen l (GRPat stmt pat id l)           -- ^ parenthesised pattern, e.g. (2*)
    | RPPat l pat                          -- ^ an ordinary pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GPat_RegList stmt pat id) where
    ann (PRPat l _) = l
    amap = fmap

instance Annotated (GPat_XRegList stmt pat id) where
    ann (PXRPats l _) = l
    amap = fmap

instance Annotated GRPatOp where
    ann (RPStar  l) = l
    ann (RPStarG l) = l
    ann (RPPlus  l) = l
    ann (RPPlusG l) = l
    ann (RPOpt   l) = l
    ann (RPOptG  l) = l
    amap = fmap

instance Annotated (GRPat stmt pat id) where
    ann rp = case rp of
      RPOp l _ _       -> l
      RPEither l _ _   -> l
      RPSeq l _        -> l
      RPGuard l _ _    -> l
      RPCAs l _ _      -> l
      RPAs l _ _       -> l
      RPParen l _      -> l
      RPPat l _        -> l
    amap = fmap
