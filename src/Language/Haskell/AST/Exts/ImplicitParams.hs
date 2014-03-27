{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.ImplicitParams

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Bind, Exp, Asst )


-- | The extension of the @Bind@ type with implicit parameters
data Bind exp id l
     = IPBinds l [IPBind exp id l]   -- ^ A binding group for implicit parameters
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A binding of an implicit parameter.
data IPBind exp id l
   = IPBind l (IPName id l) (exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An implicit parameter name.
data IPName id l
    = IPDup l id -- ^ ?/ident/, non-linear implicit parameter
    | IPLin l id -- ^ %/ident/, linear implicit parameter
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | The extension fo the @Asst@ type with implicit parameters
data Asst ty id l
     = IParam l (IPName id l) (ty id l) -- ^ implicit parameter assertion
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The extension fo the @Exp@ type with implicit parameters
data Exp id l
    = IPVar l (IPName id l) -- ^ implicit parameter variable
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Bind exp id) where
    ann (IPBinds l _)   = l

instance Annotated (IPBind exp id) where
    ann (IPBind l _ _) = l

instance Annotated (IPName id) where
    ann (IPDup l _) = l
    ann (IPLin l _) = l

instance Annotated (Asst ty id) where
    ann (IParam l _ _) = l

instance Annotated (Exp id) where
    ann (IPVar l _) = l
