{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, StandaloneDeriving #-}
module Language.Haskell2010.AST

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding (Literal,Pat,Type,Exp,Bind,Binds,Asst,TypeDecl,ClassRelatedDecl,Decl)
import qualified Language.Haskell.AST.Core as Core
import qualified Language.Haskell.AST.Sugar as Sugar
import Language.Haskell.AST.Exts.PatternGuards
import qualified Language.Haskell.AST.Exts.FFI as FFI

-- | No extensions
data NoExts id l

deriving instance Eq (NoExts id l)
deriving instance Ord (NoExts id l)
deriving instance Show (NoExts id l)
deriving instance Functor (NoExts id)
deriving instance Foldable (NoExts id)
deriving instance Traversable (NoExts id)
deriving instance Typeable NoExts
deriving instance (Data id, Data l) => Data (NoExts id l)

instance Annotated (NoExts id) where
    ann  = error "ann / Annotated NoExts"

-- | This type is used as annotation of @Literals@ in order to
--   store the exact representation
newtype ExactRep s = ExactRep { getExactRep :: s }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

type Literal = Core.Literal (ExactRep String)

-- | A Haskell 2010 pattern
type Pat = Core.Pat PatExts

data PatExts id l
    = PatSugar (Sugar.Pat Literal Pat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (PatExts id) where
    ann (PatSugar pat) = ann pat


-- | A Haskell 2010 expression
type Exp = Core.Exp LetBinds Pat Literal ExpExts

newtype ExpExts id l
  = ExpSugar (Sugar.Exp LetBinds Type Guard Pat StmtExts Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (ExpExts id) where
    ann (ExpSugar exp) = ann exp


-- | Haskell 2010 uses pattern guards
data Guard id l = PatternGuard l (Stmt id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A Haskell 2010 statement
type Stmt = Sugar.Stmt Binds Exp Pat StmtExts

type StmtExts = NoExts

-- | A Haskell 2010 assertion is of the form "C a", with a variable
--   (should be, e.g., a type without context, for FlexibleContexts...)
type Assertion = Core.Asst Name AssertionExts
type AssertionExts = NoExts

-- | A Haskell 2010 type
type Type = Core.Type TypeExts

data TypeExts id l
  = QualType  (Core.QualType Assertion Type id l)
  | TypeSugar (Sugar.Type Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (TypeExts id) where
    ann (QualType  qty) = ann qty
    ann (TypeSugar t) = ann t


-- | Haskell 2010 binds
type Bind  = Core.Bind  Type Guard Exp Pat BindExts
type Binds = Core.Binds Type Guard Exp Pat BindExts
type BindExts = NoExts

-- | This type is essentially the same as @Binds@, but we need it
--   to break the mutual recursion between @Exp@ and @Binds@
data LetBinds id l
     = LetBinds l [Bind id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (LetBinds id) where
    ann (LetBinds l _) = l

-- | Haskell 2010 assertions
type Asst = Core.Asst Name NoExts

-- | Haskell 2010 class and instance declarations
type ClassRelatedDecl = Core.ClassRelatedDecl Asst Type Bind ClassBodyExts InstBodyExts ClassRelExts

type ClassBodyExts = NoExts
type InstBodyExts  = NoExts
type ClassRelExts  = NoExts

-- | Haskell 2010 type declarations
type TypeDecl = Core.TypeDecl Asst Type TypeDeclExts
type TypeDeclExts = NoExts

-- | A Haskell 2010 module
type Module = Core.Module Bind TypeDecl ClassRelatedDecl DeclExts ModulePragmaExts


-- | Declarations
type Decl = Core.Decl Bind TypeDecl ClassRelatedDecl DeclExts
type DeclExts = FFI.Decl Type

type ModulePragmaExts = NoExts
