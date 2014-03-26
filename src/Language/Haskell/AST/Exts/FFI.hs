{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Exts.FFI

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Decl )


data Decl ty id l
     = ForImp       l (CallConv l) (Maybe (Safety l)) (Maybe String) (Name id l) (ty id l)
     -- ^ A foreign import declaration
     | ForExp       l (CallConv l)                    (Maybe String) (Name id l) (ty id l)
     -- ^ A foreign export declaration

-- | The calling convention of a foreign function call.
data CallConv l
    = StdCall l
    | CCall l
    | CPlusPlus l
    | DotNet l
    | Jvm l
    | Js l
    | CApi l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The safety of a foreign function call.
data Safety l
    = PlayRisky l         -- ^ unsafe
    | PlaySafe l Bool     -- ^ safe ('False') or threadsafe ('True')
    | PlayInterruptible l -- ^ interruptible
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (Decl ty id) where
    ann decl = case decl of
        ForImp l _ _ _ _ _  -> l
        ForExp l _ _ _ _    -> l

instance Annotated CallConv where
    ann (StdCall l) = l
    ann (CCall l) = l
    ann (CPlusPlus l) = l
    ann (DotNet l) = l
    ann (Jvm l) = l
    ann (Js l) = l
    ann (CApi l) = l

instance Annotated Safety where
    ann (PlayRisky l) = l
    ann (PlaySafe l _) = l
    ann (PlayInterruptible l) = l

