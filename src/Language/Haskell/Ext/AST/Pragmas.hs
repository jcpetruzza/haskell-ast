{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.Pragmas

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GModulePragma, GDecl, GExp )

-- | An extension of module pragmas with annotations
data GModulePragma_Ann exp id l
    = AnnModulePragma  l (GAnnotation exp id l)
                        -- ^ ANN pragma with module scope
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An annotation through an ANN pragma.
data GAnnotation exp id l
    = Ann       l (GName id l)  exp
    -- ^ An annotation for a declared name.
    | TypeAnn   l (GName id l)  exp
    -- ^ An annotation for a declared type.
    | ModuleAnn l               exp
    -- ^ An annotation for the defining module.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A RULES pragma
data GDecl_RulePragma ty exp id l
     = RulePragmaDecl   l [GRule ty exp id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The body of a RULES pragma.
data GRule ty exp id l
    = Rule l String (Maybe (GActivation l)) (Maybe [GRuleVar ty id l]) exp exp
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Variables used in a RULES pragma, optionally annotated with types
data GRuleVar ty id l
    = RuleVar l (GName id l)
    | TypedRuleVar l (GName id l) ty
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A DEPRECATED pragma
data GDecl_DeprPragma id l
     = DeprPragmaDecl l [([GName id l], String)]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A WARNING pragma
data GDecl_WarnPragma id l
     = WarnPragmaDecl l [([GName id l], String)]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An INLINE pragma
data GDecl_InlinePragma id l
     = InlineSig l Bool (Maybe (GActivation l)) (GQName id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An INLINE CONLIKE pragma
data GDecl_InlineConPragma id l
     = InlineConlikeSig l (Maybe (GActivation l)) (GQName id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A SPECIALISE pragma
data GDecl_SpecPragma ty id l
     = SpecSig l (Maybe (GActivation l)) (GQName id l) [ty]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A SPECIALISE INLINE pragma
data GDecl_SpecInlinePragma ty id l
     = SpecInlineSig l Bool (Maybe (GActivation l)) (GQName id l) [ty]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Activation clause of a RULES pragma.
data GActivation l
    = ActiveFrom   l Int
    | ActiveUntil  l Int
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A SPECIALISE instance pragma
data GDecl_SpecInstPragma insthd ctx id l
     = InstSig l (Maybe ctx) insthd
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An ANN pragma
data GDecl_AnnPragma exp id l
     = AnnPragma l (GAnnotation exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


data GDef_CorePragma exp l
    = CorePragma l String exp      -- ^ CORE pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data GDef_SCCPragma exp l
    = SCCPragma l String exp      -- ^ SCC pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data GDef_GenPragma exp l
    = GenPragma l String (Int, Int) (Int, Int) exp
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)
                                            -- ^ GENERATED pragma




instance Annotated (GModulePragma_Ann exp id) where
    ann (AnnModulePragma  l _) = l
    amap = fmap

instance Annotated (GAnnotation exp id) where
    ann (Ann     l _ _) = l
    ann (TypeAnn l _ _) = l
    ann (ModuleAnn l _) = l
    amap = fmap


instance Annotated (GDecl_RulePragma ty exp id) where
    ann (RulePragmaDecl l _) = l
    amap = fmap

instance Annotated (GRule ty exp id) where
    ann (Rule l _ _ _ _ _) = l
    amap = fmap

instance Annotated (GRuleVar ty id) where
    ann (RuleVar l _) = l
    ann (TypedRuleVar l _ _) = l
    amap = fmap

instance Annotated (GDecl_DeprPragma id) where
    ann (DeprPragmaDecl l _) = l
    amap = fmap

instance Annotated (GDecl_WarnPragma id) where
    ann (WarnPragmaDecl l _) = l
    amap = fmap

instance Annotated (GDecl_InlinePragma id) where
    ann (InlineSig l _ _ _) = l
    amap = fmap

instance Annotated (GDecl_InlineConPragma id) where
    ann (InlineConlikeSig l _ _) = l
    amap = fmap

instance Annotated (GDecl_SpecPragma ty id) where
    ann (SpecSig l _ _ _) = l
    amap = fmap

instance Annotated (GDecl_SpecInlinePragma ty id) where
    ann (SpecInlineSig l _ _ _ _) = l
    amap = fmap

instance Annotated GActivation where
    ann (ActiveFrom   l _) = l
    ann (ActiveUntil  l _) = l
    amap = fmap


instance Annotated (GDecl_SpecInstPragma insthd ctx id) where
    ann (InstSig l _ _) = l
    amap = fmap

instance Annotated (GDecl_AnnPragma exp id) where
    ann (AnnPragma l _) = l
    amap = fmap

instance Annotated (GDef_CorePragma exp) where
    ann (CorePragma l _ _)   = l
    amap = fmap

instance Annotated (GDef_SCCPragma exp) where
    ann (SCCPragma  l _ _)   = l
    amap = fmap

instance Annotated (GDef_GenPragma exp) where
    ann (GenPragma  l _ _ _ _) = l
    amap = fmap


