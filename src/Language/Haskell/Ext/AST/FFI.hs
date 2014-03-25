{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.Ext.AST.FFI

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GDecl )


data GDecl ty id l
     = ForImp       l (GCallConv l) (Maybe (GSafety l)) (Maybe String) (GName id l) (ty id l)
     -- ^ A foreign import declaration
     | ForExp       l (GCallConv l)                    (Maybe String) (GName id l) (ty id l)
     -- ^ A foreign export declaration

-- | The calling convention of a foreign function call.
data GCallConv l
    = StdCall l
    | CCall l
    | CPlusPlus l
    | DotNet l
    | Jvm l
    | Js l
    | CApi l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The safety of a foreign function call.
data GSafety l
    = PlayRisky l         -- ^ unsafe
    | PlaySafe l Bool     -- ^ safe ('False') or threadsafe ('True')
    | PlayInterruptible l -- ^ interruptible
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (GDecl ty id) where
    ann decl = case decl of
        ForImp l _ _ _ _ _  -> l
        ForExp l _ _ _ _    -> l

instance Annotated GCallConv where
    ann (StdCall l) = l
    ann (CCall l) = l
    ann (CPlusPlus l) = l
    ann (DotNet l) = l
    ann (Jvm l) = l
    ann (Js l) = l
    ann (CApi l) = l

instance Annotated GSafety where
    ann (PlayRisky l) = l
    ann (PlaySafe l _) = l
    ann (PlayInterruptible l) = l

