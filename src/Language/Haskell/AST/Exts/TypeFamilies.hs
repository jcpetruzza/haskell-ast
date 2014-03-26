{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Exts.TypeFamilies

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( GDecl )


data GDecl asst ty id l
     = TypeFamDecl  l (GDeclHead id l) (Maybe (GKind id l))
     -- ^ A type family declaration
     | DataFamDecl  l {-data-}      (Maybe (GContext asst id l)) (GDeclHead id l) (Maybe (GKind id l))
     -- ^ A data family declaration
     | TypeInsDecl  l (ty id l) (ty id l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (GDataOrNew l) (ty id l) [GQualConDecl asst ty id l] (Maybe (GDeriving ty id l))
     -- ^ A data family instance declaration
     | GDataInsDecl l (GDataOrNew l) (ty id l) (Maybe (GKind id l)) [GGadtDecl ty id l] (Maybe (GDeriving ty id l))
     -- ^ A data family instance declaration, GADT style
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data GClassDecl asst ty id l
     = ClsDataFam l (Maybe (GContext asst id l)) (GDeclHead id l) (Maybe (GKind id l))
             -- ^ declaration of an associated data type
     | ClsTyFam   l (GDeclHead id l) (Maybe (GKind id l))
             -- ^ declaration of an associated type synonym
     | ClsTyDef   l (ty id l) (ty id l)
             -- ^ default choice for an associated type synonym
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data GInstDecl asst ty id l
    = InsType   l (ty id l) (ty id l)
            -- ^ an associated type definition
    | InsData   l (GDataOrNew l) (ty id l) [GQualConDecl asst ty id l] (Maybe (GDeriving ty id l))
            -- ^ an associated data type implementation
    | InsGData  l (GDataOrNew l) (ty id l) (Maybe (GKind id l)) [GGadtDecl ty id l] (Maybe (GDeriving ty id l))
            -- ^ an associated data type implemented using GADT style
    deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


data GAsstEq ty id l
     = EqualP l (ty id l) (ty id l) -- ^ type equality constraint
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (GDecl asst ty id) where
    ann decl = case decl of
        TypeFamDecl  l _ _       -> l
        DataFamDecl  l _ _ _     -> l
        TypeInsDecl  l _ _       -> l
        DataInsDecl  l _ _ _ _   -> l
        GDataInsDecl l _ _ _ _ _ -> l

instance Annotated (GClassDecl asst ty id) where
    ann (ClsDataFam l _ _ _) = l
    ann (ClsTyFam   l    _ _) = l
    ann (ClsTyDef   l _ _) = l

instance Annotated (GInstDecl asst ty id) where
    ann insd = case insd of
        InsType   l _ _        -> l
        InsData   l _ _ _ _    -> l
        InsGData  l _ _ _ _ _  -> l

instance Annotated (GAsstEq stmt id) where
    ann (EqualP l _ _) = l
