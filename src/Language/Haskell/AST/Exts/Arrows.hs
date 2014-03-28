{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.Arrows

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Exp )

data Exp exp pat id l
     = Proc           l (pat id l) (exp id l) -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      l (exp id l) (exp id l) -- ^ arrow application (from left): /exp/ @-<@ /exp/
    | RightArrApp     l (exp id l) (exp id l) -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  l (exp id l) (exp id l) -- ^ higher-order arrow application (from left): /exp/ @-<<@ /exp/
    | RightArrHighApp l (exp id l) (exp id l) -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data Stmt stmt id l
    = RecStmt l [stmt id l] -- ^ a recursive binding group for arrows
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Exp exp pat id) where
    ann e = case e of
        Proc            l _ _ -> l
        LeftArrApp      l _ _ -> l
        RightArrApp     l _ _ -> l
        LeftArrHighApp  l _ _ -> l
        RightArrHighApp l _ _ -> l

instance Annotated (Stmt stmt id) where
    ann (RecStmt l _) = l
