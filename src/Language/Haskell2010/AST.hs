{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell2010.AST

where

import Prelude hiding (exp)

import Control.Applicative
import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding (Literal,Pat,Type,Exp,Bind,Binds,Asst,TypeDecl,ClassRelatedDecl,Decl)
import Language.Haskell.AST.Exts.NoExts
import qualified Language.Haskell.AST.Core as Core
import qualified Language.Haskell.AST.Sugar as Sugar
-- import qualified Language.Haskell.AST.Exts.PatternGuards as PatternGuards
import qualified Language.Haskell.AST.Exts.FFI as FFI

import qualified Language.Haskell.Exts.Annotated.Syntax as E
import Language.Haskell.AST.HSE

type Literal = Core.Literal

-- | A Haskell 2010 pattern
type Pat = Core.Pat PatExts

data PatExts id l
    = PatSugar (Sugar.Pat Literal Pat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (PatExts id) where
    ann (PatSugar pat) = ann pat

instance HsePat PatExts where
    fromHsePat p = PatSugar <$> fromHsePat p

-- | A Haskell 2010 expression
type Exp = Core.Exp LetBinds Pat Literal ExpExts

newtype ExpExts id l
  = ExpSugar (Sugar.Exp LetBinds Type Guard Pat StmtExts Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (ExpExts id) where
    ann (ExpSugar exp) = ann exp

instance HseExp ExpExts where
    fromHseExp e = (ExpSugar <$> fromHseExp e)
               <|> (fromHseFailed e)


-- | Haskell 2010 uses pattern guards (we define it here instead
-- of reusing the one in the PatternGuards module to avoid
-- a cyclic dependency with Stmt without introducing an additional indirection)
data Guard id l = PatternGuard l [Stmt id l]
   deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Guard id) where
    ann (PatternGuard l _) = l

instance HseGuard Guard where
    fromHseGuard l ss = PatternGuard l <$> mapM fromHseStmt ss

-- | A Haskell 2010 statement
type Stmt = Sugar.Stmt Binds Exp Pat StmtExts

type StmtExts = NoExts

-- | A Haskell 2010 type
type Type = Core.Type TypeExts

data TypeExts id l
  = QualType  (Core.QualType Asst Type id l)
  | TypeSugar (Sugar.Type Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (TypeExts id) where
    ann (QualType  qty) = ann qty
    ann (TypeSugar t) = ann t

instance HseType TypeExts where
    fromHseType t = (QualType  <$> fromHseType t)
                <|> (TypeSugar <$> fromHseType t)
                <|> (fromHseFailed t)


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

instance HseBinds LetBinds where
    fromHseBinds (E.BDecls l decls) = LetBinds l <$> mapM fromHseDecl decls
    fromHseBinds binds = fromHseFailed binds

-- | A Haskell 2010 assertion is of the form "C a", with a variable
--   (should be, e.g., a type without context, for FlexibleContexts...)
type Asst = Core.Asst Type NoExts

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
