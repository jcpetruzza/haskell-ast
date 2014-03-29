{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell98.AST

where
import Prelude hiding ( exp )
import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding (Literal,Pat,Type,Exp,Bind,Binds,Asst,TypeDecl,ClassRelatedDecl,Decl)
import Language.Haskell.AST.Exts.NoExts
import qualified Language.Haskell.AST.Core as Core
import qualified Language.Haskell.AST.Sugar as Sugar
import qualified Language.Haskell.AST.Exts.Patterns as Patterns


type Literal = Core.Literal

-- | A Haskell 98 pattern
type Pat = Core.Pat PatExts

data PatExts id l
    = PatSugar   (Sugar.Pat Literal Pat id l)
    | PatSugar98 (Patterns.NPlusKPat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (PatExts id) where
    ann (PatSugar   pat) = ann pat
    ann (PatSugar98 pat) = ann pat

-- | A Haskell 98 expression
type Exp = Core.Exp LetBinds Pat Literal ExpExts

newtype ExpExts id l
  = ExpSugar (Sugar.Exp LetBinds Type Guard Pat StmtExts Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (ExpExts id) where
    ann (ExpSugar exp) = ann exp

-- | In Haskell 98, the guards of alternatives in case-expressions are just expressions
type Guard = Exp

-- | A Haskell 98 statement
type Stmt = Sugar.Stmt Binds Exp Pat StmtExts
type StmtExts = NoExts

-- | A Haskell 98 assertion is of the form "C a", with a variable
--   (should be, e.g., a type without context, for FlexibleContexts...)
type Assertion = Core.Asst Name AssertionExts
type AssertionExts = NoExts

-- | A Haskell 98 type
type Type = Core.Type TypeExts

data TypeExts id l
  = QualType  (Core.QualType Assertion Type id l)
  | TypeSugar (Sugar.Type Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (TypeExts id) where
    ann (QualType  qty) = ann qty
    ann (TypeSugar ty)  = ann ty


-- | Haskell 98 binds
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

-- | Haskell 98 assertions
type Asst = Core.Asst Name NoExts

-- | Haskell 98 class and instance declarations
type ClassRelatedDecl = Core.ClassRelatedDecl Asst Type Bind ClassBodyExts InstBodyExts ClassRelExts

type ClassBodyExts = NoExts
type InstBodyExts  = NoExts
type ClassRelExts  = NoExts

-- | Haskell 98 type declarations
type TypeDecl = Core.TypeDecl Asst Type TypeDeclExts
type TypeDeclExts = NoExts

-- | A Haskell 98 module
type Module = Core.Module Bind TypeDecl ClassRelatedDecl DeclExts ModulePragmaExts


-- | Declarations
type Decl = Core.Decl Bind TypeDecl ClassRelatedDecl DeclExts
type DeclExts = NoExts

type ModulePragmaExts = NoExts
