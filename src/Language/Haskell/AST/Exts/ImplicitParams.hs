{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.ImplicitParams

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( GBinds, GExp, GAsst )


-- | The extension of the @GBinds@ type with implicit parameters
data GBinds exp id l
     = IPBinds l [GIPBind exp id l]   -- ^ A binding group for implicit parameters
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A binding of an implicit parameter.
data GIPBind exp id l = IPBind l (GIPName id l) exp
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An implicit parameter name.
data GIPName id l
    = IPDup l id -- ^ ?/ident/, non-linear implicit parameter
    | IPLin l id -- ^ %/ident/, linear implicit parameter
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | The extension fo the @GAsst@ type with implicit parameters
data GAsst ty id l
     = IParam l (GIPName id l) ty          -- ^ implicit parameter assertion
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The extension fo the @GExp@ type with implicit parameters
data GExp id l
    = IPVar l (GIPName id l) -- ^ implicit parameter variable
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GBinds exp id) where
    ann (IPBinds l _)   = l

instance Annotated (GIPBind exp id) where
    ann (IPBind l _ _) = l

instance Annotated (GIPName id) where
    ann (IPDup l _) = l
    ann (IPLin l _) = l

instance Annotated (GAsst ty id) where
    ann (IParam l _ _) = l

instance Annotated (GExp id) where
    ann (IPVar l _) = l
