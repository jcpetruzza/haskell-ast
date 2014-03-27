{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.TH

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding (Decl, Exp, Pat)

-- | The extension of the @GDecl@ type with TH stuff
data Decl exp id l
     = SpliceDecl l (exp id l)
     -- ^ A Template Haskell splicing declaration
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The extension of the @Pat@ type with TH stuff
data Pat id l
    = PQuasiQuote l String String           -- ^ quasi quote pattern: @[$/name/| /string/ |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | The extension of the @GExp@ type with TH stuff
data Exp decl ty exp pat id l
    = VarQuote l (QName id l)                     -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote l (QName id l)                     -- ^ @''T@ for template haskell reifying of types
    | BracketExp l (Bracket decl ty exp pat id l) -- ^ template haskell bracket expression
    | SpliceExp l (Splice exp id l)               -- ^ template haskell splice expression
    | QuasiQuote l String String                  -- ^ quasi-quotaion: @[$/name/| /string/ |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A template haskell splice expression
data Splice exp id l
    = IdSplice l String         -- ^ variable splice: @$var@
    | ParenSplice l (exp id l)  -- ^ parenthesised expression splice: @$(/exp/)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A template haskell bracket expression.
data Bracket decl ty exp pat id l
    = ExpBracket  l (exp  id l) -- ^ expression bracket: @[| ... |]@
    | PatBracket  l (pat  id l) -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket l (ty   id l) -- ^ type bracket: @[t| ... |]@
    | DeclBracket l [decl id l]  -- ^ declaration bracket: @[d| ... |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (Decl exp id) where
    ann (SpliceDecl l _) = l

instance Annotated (Pat id) where
    ann (PQuasiQuote l _ _) = l

instance Annotated (Exp decl ty exp pat id) where
    ann e = case e of
        VarQuote   l _   -> l
        TypQuote   l _   -> l
        BracketExp l _   -> l
        SpliceExp  l _   -> l
        QuasiQuote l _ _ -> l

instance Annotated (Splice exp id) where
    ann (IdSplice    l _) = l
    ann (ParenSplice l _) = l

instance Annotated (Bracket decl ty exp pat id) where
    ann (ExpBracket  l _) = l
    ann (PatBracket  l _) = l
    ann (TypeBracket l _) = l
    ann (DeclBracket l _) = l

