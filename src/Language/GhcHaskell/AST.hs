{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.GhcHaskell.AST

where

import Prelude hiding ( exp )

import Control.Applicative
import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding (Literal,Pat,Type,Exp,Bind,Binds,Asst,TypeDecl,ClassRelatedDecl,Decl)
import qualified Language.Haskell.AST.Core as Core
import qualified Language.Haskell.AST.Sugar as Sugar

import Language.Haskell.AST.Exts.NoExts

import qualified Language.Haskell.AST.Exts.Arrows as Arrows
import qualified Language.Haskell.AST.Exts.FFI as FFI
import qualified Language.Haskell.AST.Exts.GADTSyntax as GADTSyntax
import qualified Language.Haskell.AST.Exts.ImplicitParams as ImplicitParams
import qualified Language.Haskell.AST.Exts.RecursiveDo as RecursiveDo
import qualified Language.Haskell.AST.Exts.MultiParamTypeClasses as MultiParamTypeClasses
import qualified Language.Haskell.AST.Exts.ParallelListComp as ParallelListComp
-- import qualified Language.Haskell.AST.Exts.PatternGuards as PatternGuards
import qualified Language.Haskell.AST.Exts.Patterns as Patterns
import qualified Language.Haskell.AST.Exts.Pragmas as Pragmas
import qualified Language.Haskell.AST.Exts.StandaloneDeriving as StandaloneDeriving
import qualified Language.Haskell.AST.Exts.TH as TH
import qualified Language.Haskell.AST.Exts.TransformListComp as TransformListComp
import qualified Language.Haskell.AST.Exts.TypeFamilies as TypeFamilies
import qualified Language.Haskell.AST.Exts.ViewPatterns as ViewPatterns

import qualified Language.Haskell.Exts.Annotated.Syntax as E
import Language.Haskell.AST.HSE

type Literal = Core.Literal

-- | Patterns
type Pat = Core.Pat PatExts

data PatExts id l
    = PatSugar     (Sugar.Pat Literal Pat id l)
    | BangPatterns (Patterns.BangPat Pat id l)
    | PatTySig     (Patterns.PatTySig Type Pat id l)
    | PatExplTyArg (Patterns.PatExplTyArg Type id l)
    | NPlusKPat    (Patterns.NPlusKPat  id l)
    | THPat        (TH.Pat id l)
    | ViewPat      (ViewPatterns.Pat Exp Pat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (PatExts id) where
    ann p = case p of
      PatSugar     pat -> ann pat
      BangPatterns pat -> ann pat
      PatTySig     pat -> ann pat
      PatExplTyArg pat -> ann pat
      NPlusKPat    pat -> ann pat
      THPat        pat -> ann pat
      ViewPat      pat -> ann pat

instance HsePat PatExts where
    fromHsePat p = (PatSugar     <$> fromHsePat p)
               <|> (BangPatterns <$> fromHsePat p)
               <|> (PatTySig     <$> fromHsePat p)
               <|> (PatExplTyArg <$> fromHsePat p)
               <|> (NPlusKPat    <$> fromHsePat p)
               <|> (THPat        <$> fromHsePat p)
               <|> (ViewPat      <$> fromHsePat p)
               <|> (fromHseFailed p)

-- | Expressions
type Exp = Core.Exp LetBinds Pat Literal ExpExts

data ExpExts id l
  = ExpSugar     (Sugar.Exp LetBinds Type Guard Pat StmtExts Exp id l)
  | ExpArr       (Arrows.Exp Exp Pat id l)
  | ExpIP        (ImplicitParams.Exp id l)
  | ExpParList   (ParallelListComp.Exp (TransformListComp.QualStmt Stmt Exp) Exp id l)
  | ExpEListComp (TransformListComp.Exp Stmt Exp id l)
  | ExpPragma    (PragmaExp id l)
  | ExpTH        (TH.Exp Decl Type Exp Pat id l)
  | ExpMDo       (RecursiveDo.Exp Stmt id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data PragmaExp id l
  = PE1 (Pragmas.Exp_CorePragma Exp id l)
  | PE2 (Pragmas.Exp_SCCPragma  Exp id l)
  | PE3 (Pragmas.Exp_GenPragma  Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (ExpExts id) where
    ann e = case e of
      ExpSugar     exp -> ann exp
      ExpArr       exp -> ann exp
      ExpIP        exp -> ann exp
      ExpParList   exp -> ann exp
      ExpEListComp exp -> ann exp
      ExpPragma    exp -> ann exp
      ExpTH        exp -> ann exp
      ExpMDo       exp -> ann exp

instance Annotated (PragmaExp id) where
     ann p = case p of
        PE1 pr -> ann pr
        PE2 pr -> ann pr
        PE3 pr -> ann pr

instance HseExp ExpExts where
   fromHseExp e = (ExpSugar     <$> fromHseExp e)
              <|> (ExpArr       <$> fromHseExp e)
              <|> (ExpIP        <$> fromHseExp e)
              <|> (ExpParList   <$> fromHseExp e)
              <|> (ExpEListComp <$> fromHseExp e)
              <|> (ExpPragma    <$> fromHseExp e)
              <|> (ExpTH        <$> fromHseExp e)
              <|> (ExpMDo       <$> fromHseExp e)
              <|> (fromHseFailed e)

instance HseExp PragmaExp where
   fromHseExp e = (PE1 <$> fromHseExp e)
              <|> (PE2 <$> fromHseExp e)
              <|> (PE3 <$> fromHseExp e)
              <|> (fromHseExp e)

-- | GhcHaskell allows pattern guards (we define it here instead
-- of reusing the one in the PatternGuards module to avoid
-- a cyclic dependency with Stmt without introducing an additional indirection)
data Guard id l = PatternGuard l [Stmt id l]
   deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (Guard id) where
    ann (PatternGuard l _) = l

instance HseGuard Guard where
    fromHseGuard l ss = PatternGuard l <$> mapM fromHseStmt ss

-- | Statements
type Stmt = Sugar.Stmt Binds Exp Pat StmtExts

newtype StmtExts id l
     = StmtArr (Arrows.Stmt Stmt id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (StmtExts id) where
  ann (StmtArr stmt) = ann stmt

instance HseStmt StmtExts where
  fromHseStmt s = (StmtArr <$> fromHseStmt s)

-- | Types
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

-- | Binds
type Bind = Core.Bind  Type Guard Exp Pat BindExts

data Binds id l
   = NormalBinds (Core.Binds Type Guard Exp Pat BindExts id l)
   | IPBinds (ImplicitParams.Binds Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

type BindExts = NoExts

instance Annotated (Binds id) where
  ann (NormalBinds b) = ann b
  ann (IPBinds     b) = ann b

instance HseBinds Binds where
  fromHseBinds b = (NormalBinds <$> fromHseBinds b)
               <|> (IPBinds     <$> fromHseBinds b)
               <|> (fromHseFailed b)

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

-- | Assertions...
type Asst = Core.Asst Type AsstExts

data AsstExts id l
     = AsstIP   (ImplicitParams.Asst Type id l)
     | AsstMPTC (MultiParamTypeClasses.AsstInfix Type id l)
     | AsstTF   (TypeFamilies.AsstEq Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance  Annotated (AsstExts id) where
  ann a = case a of
    AsstIP   asst -> ann asst
    AsstMPTC asst -> ann asst
    AsstTF   asst -> ann asst

instance HseAsst AsstExts where
  fromHseAsst a = (AsstIP   <$> fromHseAsst a)
              <|> (AsstMPTC <$> fromHseAsst a)
              <|> (AsstTF   <$> fromHseAsst a)
              <|> (fromHseFailed a)

-- | Class / instance declarations
type ClassRelatedDecl = Core.ClassRelatedDecl Asst Type Bind ClassBodyExts InstBodyExts ClassRelExts

type ClassBodyExts = TypeFamilies.ClassBody Asst Type
type InstBodyExts  = TypeFamilies.InstBody  Asst Type
type ClassRelExts  = StandaloneDeriving.ClassRelatedDecl Asst Type

-- | Type declarations
type TypeDecl = Core.TypeDecl Asst Type TypeDeclExts
newtype TypeDeclExts id l = GADT (GADTSyntax.TypeDecl Asst Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (TypeDeclExts id) where
  ann (GADT d) = ann d

instance HseDecl TypeDeclExts where
  fromHseDecl d = GADT <$> fromHseDecl d

-- | Modules
type Module = Core.Module Bind TypeDecl ClassRelatedDecl DeclExts ModulePragmaExts

type ModulePragmaExts = Pragmas.ModulePragma_Ann Exp

-- | Declarations
type Decl = Core.Decl Bind TypeDecl ClassRelatedDecl DeclExts

data DeclExts id l
     = DeclPragma (PragmaDecl id l)
     | DeclTH     (TH.Decl Exp id l)
     | DeclTF     (TypeFamilies.Decl Asst Type id l)
     | DeclFFI    (FFI.Decl Type id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


data PragmaDecl id l
     = PD1 (Pragmas.Decl_RulePragma Type Exp id l)
     | PD2 (Pragmas.Decl_DeprPragma id l)
     | PD3 (Pragmas.Decl_WarnPragma id l)
     | PD4 (Pragmas.Decl_InlinePragma id l)
     | PD5 (Pragmas.Decl_InlineConPragma id l)
     | PD6 (Pragmas.Decl_SpecPragma Type id l)
     | PD7 (Pragmas.Decl_SpecInlinePragma Type id l)
     | PD8 (Pragmas.Decl_SpecInstPragma Asst Type id l)
     | PD9 (Pragmas.Decl_AnnPragma Exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

instance Annotated (DeclExts id) where
  ann (DeclPragma d) = ann d
  ann (DeclTH     d) = ann d
  ann (DeclTF     d) = ann d
  ann (DeclFFI    d) = ann d


instance Annotated (PragmaDecl id) where
  ann pd = case pd of
    PD1 p -> ann p
    PD2 p -> ann p
    PD3 p -> ann p
    PD4 p -> ann p
    PD5 p -> ann p
    PD6 p -> ann p
    PD7 p -> ann p
    PD8 p -> ann p
    PD9 p -> ann p

instance HseDecl DeclExts where
  fromHseDecl d = (DeclPragma <$> fromHseDecl d)
              <|> (DeclTH     <$> fromHseDecl d)
              <|> (DeclTF     <$> fromHseDecl d)
              <|> (DeclFFI    <$> fromHseDecl d)
              <|> (fromHseFailed d)

instance HseDecl PragmaDecl where
  fromHseDecl d = (PD1 <$> fromHseDecl d)
              <|> (PD2 <$> fromHseDecl d)
              <|> (PD3 <$> fromHseDecl d)
              <|> (PD4 <$> fromHseDecl d)
              <|> (PD5 <$> fromHseDecl d)
              <|> (PD6 <$> fromHseDecl d)
              <|> (PD7 <$> fromHseDecl d)
              <|> (PD8 <$> fromHseDecl d)
              <|> (PD9 <$> fromHseDecl d)
              <|> (fromHseFailed d)
