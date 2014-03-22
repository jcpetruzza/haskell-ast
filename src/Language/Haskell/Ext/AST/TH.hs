{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.TH

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding (GDecl, GExp, GPat)

-- | The extension of the @GDecl@ type with TH stuff
data GDecl exp l
     = SpliceDecl   l exp
     -- ^ A Template Haskell splicing declaration
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The extension of the @GPat@ type with TH stuff
data GPat lit id l
    = PQuasiQuote l String String           -- ^ quasi quote pattern: @[$/name/| /string/ |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | The extension of the @GExp@ type with TH stuff
data GExp decl ty exp pat id l
    = VarQuote l (GQName id l)                  -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote l (GQName id l)                  -- ^ @''T@ for template haskell reifying of types
    | BracketExp l (GBracket decl ty exp pat id l)              -- ^ template haskell bracket expression
    | SpliceExp l (GSplice exp id l)                -- ^ template haskell splice expression
    | QuasiQuote l String String            -- ^ quasi-quotaion: @[$/name/| /string/ |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A template haskell splice expression
data GSplice exp id l
    = IdSplice l String           -- ^ variable splice: @$var@
    | ParenSplice l exp       -- ^ parenthesised expression splice: @$(/exp/)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A template haskell bracket expression.
data GBracket decl ty exp pat id l
    = ExpBracket l exp  -- ^ expression bracket: @[| ... |]@
    | PatBracket l pat                -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket l ty     -- ^ type bracket: @[t| ... |]@
    | DeclBracket l [decl]  -- ^ declaration bracket: @[d| ... |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (GDecl exp) where
    ann (SpliceDecl l _) = l
    amap = fmap

instance Annotated (GPat lit id) where
    ann (PQuasiQuote l _ _) = l
    amap = fmap

instance Annotated (GExp decl ty exp pat id) where
    ann e = case e of
        VarQuote l _           -> l
        TypQuote l _           -> l
        BracketExp l _         -> l
        SpliceExp l _          -> l
        QuasiQuote l _ _      -> l
    amap = fmap

instance Annotated (GSplice exp id) where
    ann (IdSplice l _) = l
    ann (ParenSplice l _) = l
    amap = fmap

instance Annotated (GBracket decl ty exp pat id) where
    ann (ExpBracket l _) = l
    ann (PatBracket l _) = l
    ann (TypeBracket l _) = l
    ann (DeclBracket l _) = l
    amap = fmap

