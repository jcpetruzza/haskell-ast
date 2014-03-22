{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST
   (
    module Language.Haskell.AST
   ,module Language.Haskell.Ext.AST
   )

where

import Control.Applicative

import Data.Data
import Data.Generics (Data(..),Typeable(..))
import Data.Foldable (Foldable(..))
import Data.Monoid
import Data.Traversable


import Language.Haskell.AST hiding ( GBinds(..) )
import qualified Language.Haskell.AST.Sugar as Sugar

import qualified Language.Haskell.Ext.AST.HaRP as HaRP
import qualified Language.Haskell.Ext.AST.HSX as HSX
import qualified Language.Haskell.Ext.AST.ViewPatterns as ViewPatterns
import qualified Language.Haskell.Ext.AST.Pragmas as Pragmas
import qualified Language.Haskell.Ext.AST.ImplicitParams as ImplicitParams
import qualified Language.Haskell.Ext.AST.TH as TH
import qualified Language.Haskell.Ext.AST.ParallelListComp as ParallelListComp
import qualified Language.Haskell.Ext.AST.TransformListComp as TransformListComp
import qualified Language.Haskell.Ext.AST.Arrows as Arrows
import qualified Language.Haskell.Ext.AST.MDo as MDo
import qualified Language.Haskell.Ext.AST.PatternGuards as PatternGuards
import qualified Language.Haskell.Ext.AST.Patterns as Patterns


type ModuleName id l = GModuleName id l
type SpecialCon l = GSpecialCon l
type QName id l = GQName id l
type Name id l = GName id l
type Op id l = GOp id l
type CName id l = GCName id l
type Module id l = GModule (Exp id l) (Pat id l) id l
type ModuleHead id l = GModuleHead id l
type ExportSpecList id l = GExportSpecList id l
type ExportSpec id l = GExportSpec id l
type ImportDecl id l = GImportDecl id l
type ImportSpecList id l = GImportSpecList id l
type ImportSpec id l = GImportSpec id l
type Assoc l = GAssoc l
type Decl id l = GDecl (Pat id l) id l
type DataOrNew l = GDataOrNew l
type DeclHead id l = GDeclHead id l
type InstHead id l = GInstHead id l
type Deriving id l = GDeriving id l
type Match id l = GMatch (Exp id l) (Pat id l) id l
type QualConDecl id l = GQualConDecl id l
type ConDecl id l = GConDecl id l
type FieldDecl id l = GFieldDecl id l
type GadtDecl id l = GGadtDecl id l
type ClassDecl id l = GClassDecl (Exp id l) (Pat id l) id l
type InstDecl id l = GInstDecl (Exp id l) (Pat id l) id l
type BangType id l = GBangType id l
type Rhs id l = GRhs (Exp id l) (Pat id l) id l
type GuardedRhs id l = GGuardedRhs (Pat id l) id l
type Type id l = GType id l
-- type Promoted id l = GPromoted id l
type TyVarBind id l = GTyVarBind id l
type Kind id l = GKind id l
type FunDep id l = GFunDep id l
type Context id l = GContext id l
type Asst id l = GAsst id l
type Literal = GLiteral (ExactRep String)
type Exp id l = GExp (Binds id l) (Pat id l) Literal id l

type Safety l = GSafety l
type CallConv l = GCallConv l
type ModulePragma id l = GModulePragma id l
type WarningText l = GWarningText l
type Pat id l = GPat Literal id l

type Stmt id l = GStmt (Exp id l) (Pat id l) id l
type Alt id l = GAlt (Exp id l) (Pat id l) id l
type GuardedAlts id l = GGuardedAlts (Pat id l) id l
type GuardedAlt id l = GGuardedAlt (Pat id l) id l

-- | A binding group inside a @let@ or @where@ clause.
--   We define it here to break the dependency between expressions and declarations
data Binds id l
    = BDecls  l [GDecl (Exp id l) (Pat id l) id l]     -- ^ An ordinary binding group
  deriving (Eq,Ord,Show,Typeable,Data)


{- TODO: The derived instances of Traversable et al are not doing what
    one would expect (should be mostly sorted out once many dependencies are
    on extensions instead of full types)
instance Functor (Binds id) where
    fmap = fmapDefault

instance Foldable (Binds id) where
    foldMap = foldMapDefault

instance Traversable (Binds id) where
    traverse f (BDecls l decls)
      = BDecls <$> f l <*> traverse f decls

instance Annotated (Binds id) where
    ann (BDecls  l _) = l
    amap = fmap

-}

-- TODO: Sugar

-- TODO: HSX stuff
-- type XName id l = GXName id l
-- type XAttr id l = GXAttr id l

-- TODO: HaRP stuff
--type PXAttr id l = GPXAttr (Pat id l) id l
--type RPatOp l = GRPatOp l
--type RPat id l = GRPat (Pat id l) id l

-- TODO: ViewPatterns stuff

-- TODO: Pragmas

-- TODO: TH stuff
-- TODO: ParallelListComp and TransformListComp stuff

-- TODO: Arrows stuff

-- TODO: MDo

-- TODO: PatternGuards and other pattern extensions

-- | This type is used as annotation of @Literals@ in order to
--   store the exact representation
newtype ExactRep s = ExactRep { getExactRep :: s }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data NoExts