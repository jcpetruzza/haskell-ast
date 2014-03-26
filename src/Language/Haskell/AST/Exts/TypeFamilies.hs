{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Exts.TypeFamilies

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Decl )


data Decl asst ty id l
     = TypeFamDecl  l (DeclHead id l) (Maybe (Kind id l))
     -- ^ A type family declaration
     | DataFamDecl  l {-data-}      (Maybe (Context asst id l)) (DeclHead id l) (Maybe (Kind id l))
     -- ^ A data family declaration
     | TypeInsDecl  l (ty id l) (ty id l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (DataOrNew l) (ty id l) [QualConDecl asst ty id l] (Maybe (Deriving ty id l))
     -- ^ A data family instance declaration
     | GDataInsDecl l (DataOrNew l) (ty id l) (Maybe (Kind id l)) [GadtDecl ty id l] (Maybe (Deriving ty id l))
     -- ^ A data family instance declaration, ADT style
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data ClassDecl asst ty id l
     = ClsDataFam l (Maybe (Context asst id l)) (DeclHead id l) (Maybe (Kind id l))
             -- ^ declaration of an associated data type
     | ClsTyFam   l (DeclHead id l) (Maybe (Kind id l))
             -- ^ declaration of an associated type synonym
     | ClsTyDef   l (ty id l) (ty id l)
             -- ^ default choice for an associated type synonym
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data InstDecl asst ty id l
    = InsType   l (ty id l) (ty id l)
            -- ^ an associated type definition
    | InsData   l (DataOrNew l) (ty id l) [QualConDecl asst ty id l] (Maybe (Deriving ty id l))
            -- ^ an associated data type implementation
    | InsGData  l (DataOrNew l) (ty id l) (Maybe (Kind id l)) [GadtDecl ty id l] (Maybe (Deriving ty id l))
            -- ^ an associated data type implemented using GADT style
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


data AsstEq ty id l
     = EqualP l (ty id l) (ty id l) -- ^ type equality constraint
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Decl asst ty id) where
    ann decl = case decl of
        TypeFamDecl  l _ _       -> l
        DataFamDecl  l _ _ _     -> l
        TypeInsDecl  l _ _       -> l
        DataInsDecl  l _ _ _ _   -> l
        GDataInsDecl l _ _ _ _ _ -> l

instance Annotated (ClassDecl asst ty id) where
    ann (ClsDataFam l _ _ _) = l
    ann (ClsTyFam   l    _ _) = l
    ann (ClsTyDef   l _ _) = l

instance Annotated (InstDecl asst ty id) where
    ann insd = case insd of
        InsType   l _ _        -> l
        InsData   l _ _ _ _    -> l
        InsGData  l _ _ _ _ _  -> l

instance Annotated (AsstEq stmt id) where
    ann (EqualP l _ _) = l
