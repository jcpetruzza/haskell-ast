module Language.Haskell.Ext.AST
   (
    module Language.Haskell.AST
   ,module Language.Haskell.Ext.AST
   )

where

import Language.Haskell.AST


type ModuleName id l = GModuleName id l
type SpecialCon l = GSpecialCon l
type QName id l = GQName id l
type Name id l = GName id l
type IPName id l = GIPName id l
type QOp id l = GQOp id l
type Op id l = GOp id l
type CName id l = GCName id l
type Module id l = GModule id l
type ModuleHead id l = GModuleHead id l
type ExportSpecList id l = GExportSpecList id l
type ExportSpec id l = GExportSpec id l
type ImportDecl id l = GImportDecl id l
type ImportSpecList id l = GImportSpecList id l
type ImportSpec id l = GImportSpec id l
type Assoc l = GAssoc l
type Decl id l = GDecl id l
type Annotation id l = GAnnotation id l
type DataOrNew l = GDataOrNew l
type DeclHead id l = GDeclHead id l
type InstHead id l = GInstHead id l
type Deriving id l = GDeriving id l
type Binds id l = GBinds id l
type IPBind id l = GIPBind id l
type Match id l = GMatch id l
type QualConDecl id l = GQualConDecl id l
type ConDecl id l = GConDecl id l
type FieldDecl id l = GFieldDecl id l
type GadtDecl id l = GGadtDecl id l
type ClassDecl id l = GClassDecl id l
type InstDecl id l = GInstDecl id l
type BangType id l = GBangType id l
type Rhs id l = GRhs id l
type GuardedRhs id l = GGuardedRhs id l
type Type id l = GType id l
-- type Promoted id l = GPromoted id l
type TyVarBind id l = GTyVarBind id l
type Kind id l = GKind id l
type FunDep id l = GFunDep id l
type Context id l = GContext id l
type Asst id l = GAsst id l
type Literal l = GLiteral l
type Exp id l = GExp id l
type XName id l = GXName id l
type XAttr id l = GXAttr id l
type Bracket id l = GBracket id l
type Splice id l = GSplice id l
type Safety l = GSafety l
type CallConv l = GCallConv l
type ModulePragma id l = GModulePragma id l
type Activation l = GActivation l
type Rule id l = GRule id l
type RuleVar id l = GRuleVar id l
type WarningText l = GWarningText l
type Pat id l = GPat id l
type PXAttr id l = GPXAttr id l
type RPatOp l = GRPatOp l
type RPat id l = GRPat id l
type PatField id l = GPatField id l
type Stmt id l = GStmt id l
type QualStmt id l = GQualStmt id l
type FieldUpdate id l = GFieldUpdate id l
type Alt id l = GAlt id l
type GuardedAlts id l = GGuardedAlts id l
type GuardedAlt id l = GGuardedAlt id l
-- type IfAlt id l = GIfAlt id l


data NoExts