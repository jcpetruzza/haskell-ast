{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.Pragmas

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( ModulePragma, Decl, Exp )

-- | An extension of module pragmas with annotations
data ModulePragma_Ann exp id l
    = AnnModulePragma  l (Annotation exp id l)
                        -- ^ ANN pragma with module scope
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An annotation through an ANN pragma.
data Annotation exp id l
    = Ann       l (Name id l)  (exp id l)
    -- ^ An annotation for a declared name.
    | TypeAnn   l (Name id l)  (exp id l)
    -- ^ An annotation for a declared type.
    | ModuleAnn l              (exp id l)
    -- ^ An annotation for the defining module.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A RULES pragma
data Decl_RulePragma ty exp id l
     = RulePragmaDecl   l [Rule ty exp id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The body of a RULES pragma.
data Rule ty exp id l
    = Rule l String (Maybe (Activation l)) (Maybe [RuleVar ty id l]) (exp id l) (exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Variables used in a RULES pragma, optionally annotated with types
data RuleVar ty id l
    = RuleVar l (Name id l)
    | TypedRuleVar l (Name id l) (ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A DEPRECATED pragma
data Decl_DeprPragma id l
     = DeprPragmaDecl l [([Name id l], String)]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A WARNING pragma
data Decl_WarnPragma id l
     = WarnPragmaDecl l [([Name id l], String)]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An INLINE pragma
data Decl_InlinePragma id l
     = InlineSig l Bool (Maybe (Activation l)) (QName id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An INLINE CONLIKE pragma
data Decl_InlineConPragma id l
     = InlineConlikeSig l (Maybe (Activation l)) (QName id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A SPECIALISE pragma
data Decl_SpecPragma ty id l
     = SpecSig l (Maybe (Activation l)) (QName id l) [ty id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A SPECIALISE INLINE pragma
data Decl_SpecInlinePragma ty id l
     = SpecInlineSig l Bool (Maybe (Activation l)) (QName id l) [ty id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Activation clause of a RULES pragma.
data Activation l
    = ActiveFrom   l Int
    | ActiveUntil  l Int
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A SPECIALISE instance pragma
data Decl_SpecInstPragma insthd ctx id l
     = InstSig l (Maybe (ctx id l)) (insthd id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An ANN pragma
data Decl_AnnPragma exp id l
     = AnnPragma l (Annotation exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


data Exp_CorePragma exp id l
    = CorePragma l String (exp id l)      -- ^ CORE pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data Exp_SCCPragma exp id l
    = SCCPragma l String (exp id l)      -- ^ SCC pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data Exp_GenPragma exp id l
    = GenPragma l String (Int, Int) (Int, Int) (exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)
                                            -- ^ GENERATED pragma



instance Annotated (ModulePragma_Ann exp id) where
    ann (AnnModulePragma  l _) = l

instance Annotated (Annotation exp id) where
    ann (Ann     l _ _) = l
    ann (TypeAnn l _ _) = l
    ann (ModuleAnn l _) = l


instance Annotated (Decl_RulePragma ty exp id) where
    ann (RulePragmaDecl l _) = l

instance Annotated (Rule ty exp id) where
    ann (Rule l _ _ _ _ _) = l

instance Annotated (RuleVar ty id) where
    ann (RuleVar l _) = l
    ann (TypedRuleVar l _ _) = l

instance Annotated (Decl_DeprPragma id) where
    ann (DeprPragmaDecl l _) = l

instance Annotated (Decl_WarnPragma id) where
    ann (WarnPragmaDecl l _) = l

instance Annotated (Decl_InlinePragma id) where
    ann (InlineSig l _ _ _) = l

instance Annotated (Decl_InlineConPragma id) where
    ann (InlineConlikeSig l _ _) = l

instance Annotated (Decl_SpecPragma ty id) where
    ann (SpecSig l _ _ _) = l

instance Annotated (Decl_SpecInlinePragma ty id) where
    ann (SpecInlineSig l _ _ _ _) = l

instance Annotated Activation where
    ann (ActiveFrom   l _) = l
    ann (ActiveUntil  l _) = l


instance Annotated (Decl_SpecInstPragma insthd ctx id) where
    ann (InstSig l _ _) = l

instance Annotated (Decl_AnnPragma exp id) where
    ann (AnnPragma l _) = l

instance Annotated (Exp_CorePragma exp id) where
    ann (CorePragma l _ _)   = l

instance Annotated (Exp_SCCPragma exp id) where
    ann (SCCPragma  l _ _)   = l

instance Annotated (Exp_GenPragma exp id) where
    ann (GenPragma  l _ _ _ _) = l


