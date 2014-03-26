{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.Arrows

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( GExp, GStmt )

data GExp exp pat l
     = Proc           l pat exp     -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      l exp exp     -- ^ arrow application (Gfrom left): /exp/ @-<@ /exp/
    | RightArrApp     l exp exp     -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  l exp exp     -- ^ higher-order arrow application (Gfrom left): /exp/ @-<<@ /exp/
    | RightArrHighApp l exp exp     -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data GStmt stmt l
    = RecStmt l [stmt]    -- ^ a recursive binding group for arrows
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GExp exp pat) where
    ann e = case e of
        Proc            l _ _ -> l
        LeftArrApp      l _ _ -> l
        RightArrApp     l _ _ -> l
        LeftArrHighApp  l _ _ -> l
        RightArrHighApp l _ _ -> l

instance Annotated (GStmt stmt) where
    ann (RecStmt l _) = l
