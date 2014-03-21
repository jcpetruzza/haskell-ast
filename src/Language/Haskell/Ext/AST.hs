module Language.Haskell.Ext.AST
   (
    -- * Modules
    Module(..), ModuleHead(..), WarningText(..), ExportSpecList(..), ExportSpec(..),
    ImportDecl(..), ImportSpecList(..), ImportSpec(..), Assoc(..),
    -- * Declarations
    Decl(..), DeclHead(..), InstHead(..), Binds(..), IPBind(..),
    -- ** Type classes and instances
    ClassDecl(..), InstDecl(..), Deriving(..),
    -- ** Data type declarations
    DataOrNew(..), ConDecl(..), FieldDecl(..), QualConDecl(..), GadtDecl(..), BangType(..),
    -- ** Function bindings
    Match(..), Rhs(..), GuardedRhs(..),
    -- * Class Assertions and Contexts
    Context(..), FunDep(..), Asst(..),
    -- * Types
    Type(..), B.Boxed(..), Kind(..), TyVarBind(..), -- Promoted(..),
    -- * Expressions
    Exp(..), Stmt(..), QualStmt(..), FieldUpdate(..),
    Alt(..), GuardedAlts(..), GuardedAlt(..), XAttr(..), -- IfAlt(..),
    -- * Patterns
    Pat(..), PatField(..), PXAttr(..), RPat(..), RPatOp(..),
    -- * Literals
    Literal(..),
    -- * Variables, Constructors and Operators
    ModuleName(..), QName(..), Name(..), QOp(..), Op(..),
    SpecialCon(..), CName(..), IPName(..), XName(..),

    -- * Template Haskell
    Bracket(..), Splice(..),

    -- * FFI
    Safety(..), CallConv(..),

    -- * Pragmas
    ModulePragma(..), B.Tool(..),
    Rule(..), RuleVar(..), Activation(..),
    Annotation(..)
  )

where

import qualified Language.Haskell.AST as B


type ModuleName id l = B.ModuleName l id
type SpecialCon l = B.SpecialCon l
type QName id l = B.QName id l
type Name id l = B.Name id l
type IPName id l = B.IPName id l
type QOp id l = B.QOp id l
type Op id l = B.Op id l
type CName id l = B.CName id l
type Module id l = B.Module id l
type ModuleHead id l = B.ModuleHead id l
type ExportSpecList id l = B.ExportSpecList id l
type ExportSpec id l = B.ExportSpec id l
type ImportDecl id l = B.ImportDecl id l
type ImportSpecList id l = B.ImportSpecList id l
type ImportSpec id l = B.ImportSpec id l
type Assoc l = B.Assoc l
type Decl id l = B.Decl id l
type Annotation id l = B.Annotation id l
type DataOrNew l = B.DataOrNew l
type DeclHead id l = B.DeclHead id l
type InstHead id l = B.InstHead id l
type Deriving id l = B.Deriving id l
type Binds id l = B.Binds id l
type IPBind id l = B.IPBind id l
type Match id l = B.Match id l
type QualConDecl id l = B.QualConDecl id l
type ConDecl id l = B.ConDecl id l
type FieldDecl id l = B.FieldDecl id l
type GadtDecl id l = B.GadtDecl id l
type ClassDecl id l = B.ClassDecl id l
type InstDecl id l = B.InstDecl id l
type BangType id l = B.BangType id l
type Rhs id l = B.Rhs id l
type GuardedRhs id l = B.GuardedRhs id l
type Type id l = B.Type id l
-- type Promoted id l = B.Promoted id l
type TyVarBind id l = B.TyVarBind id l
type Kind id l = B.Kind id l
type FunDep id l = B.FunDep id l
type Context id l = B.Context id l
type Asst id l = B.Asst id l
type Literal l = B.Literal l
type Exp id l = B.Exp id l
type XName id l = B.XName id l
type XAttr id l = B.XAttr id l
type Bracket id l = B.Bracket id l
type Splice id l = B.Splice id l
type Safety l = B.Safety l
type CallConv l = B.CallConv l
type ModulePragma id l = B.ModulePragma id l
type Activation l = B.Activation l
type Rule id l = B.Rule id l
type RuleVar id l = B.RuleVar id l
type WarningText l = B.WarningText l
type Pat id l = B.Pat id l
type PXAttr id l = B.PXAttr id l
type RPatOp l = B.RPatOp l
type RPat id l = B.RPat id l
type PatField id l = B.PatField id l
type Stmt id l = B.Stmt id l
type QualStmt id l = B.QualStmt id l
type FieldUpdate id l = B.FieldUpdate id l
type Alt id l = B.Alt id l
type GuardedAlts id l = B.GuardedAlts id l
type GuardedAlt id l = B.GuardedAlt id l
-- type IfAlt id l = B.IfAlt id l


data NoExts