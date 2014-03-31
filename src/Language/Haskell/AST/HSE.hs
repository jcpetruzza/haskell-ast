{-# OPTIONS_GHC -fno-warn-orphans #-} -- due to instance Alternative ParseResult
module Language.Haskell.AST.HSE
   (
    ParseResult(..)
   ,Parsed, Parsed_

   ,HseModule(..)
   ,HseDecl(..)
   ,HseBinds(..)
   ,HseClassDecl(..)
   ,HseInstDecl(..)
   ,HseType(..)
   ,HseAsst(..)
   ,HseLiteral(..)
   ,HseExp(..)
   ,HseModulePragma(..)
   ,HsePat(..)
   ,HseStmt(..)
   ,HseQualStmt(..)
   ,HseGuard(..)
   ,fromHseFailed

   ,module Language.Haskell.Exts.SrcLoc
   )

where


import Control.Applicative
import Data.Data
import Data.Traversable ( traverse )

import qualified Language.Haskell.Exts.Annotated.Syntax as E
import Language.Haskell.Exts.Parser ( ParseResult(..) )
import Language.Haskell.Exts.SrcLoc

import qualified Language.Haskell.AST.Core  as Core
import qualified Language.Haskell.AST.Sugar as Sugar

import Language.Haskell.AST.Exts.NoExts

import qualified Language.Haskell.AST.Exts.Arrows as Arrows
import qualified Language.Haskell.AST.Exts.FFI as FFI
import qualified Language.Haskell.AST.Exts.HSX as HSX
import qualified Language.Haskell.AST.Exts.HaRP as HaRP
import qualified Language.Haskell.AST.Exts.ImplicitParams as ImplicitParams
import qualified Language.Haskell.AST.Exts.GADTSyntax as GADTSyntax
import qualified Language.Haskell.AST.Exts.RecursiveDo as RecursiveDo
import qualified Language.Haskell.AST.Exts.MultiParamTypeClasses as MultiParamTypeClasses
import qualified Language.Haskell.AST.Exts.ParallelListComp as ParallelListComp
import qualified Language.Haskell.AST.Exts.PatternGuards as PatternGuards
import qualified Language.Haskell.AST.Exts.Patterns as Patterns
import qualified Language.Haskell.AST.Exts.Pragmas as Pragmas
import qualified Language.Haskell.AST.Exts.StandaloneDeriving as StandaloneDeriving
import qualified Language.Haskell.AST.Exts.TH as TH
import qualified Language.Haskell.AST.Exts.TransformListComp as TransformListComp
import qualified Language.Haskell.AST.Exts.TypeFamilies as TypeFamilies
import qualified Language.Haskell.AST.Exts.ViewPatterns as ViewPatterns

type Parsed t = t ParsedId SrcLoc
type ParsedId = String

type Parsed_ t = t SrcLoc

instance Alternative ParseResult where
    empty = ParseFailed noLoc "empty"

    a <|> b = case a of
        ParseOk{}     -> a
        ParseFailed{} -> case b of
                             ParseFailed x "empty"| x == noLoc -> a
                             _ -> b

fromHseFailed :: (E.Annotated a, Data (a SrcLoc), Typeable b) => a SrcLoc -> ParseResult b
fromHseFailed a = res
  where msg = unwords ["Constructor", show $ toConstr a, "could not be converted to type", show $ typeOf b]
        res = ParseFailed (E.ann a) msg
        ~(ParseOk b) = res



fromHseModuleName :: Parsed_ E.ModuleName -> Parsed Core.ModuleName
fromHseModuleName (E.ModuleName l s) = Core.ModuleName l s

fromHseSpecialCon :: Parsed_ E.SpecialCon -> Parsed_ Core.SpecialCon
fromHseSpecialCon sc = case sc of
        E.UnitCon l       -> Core.UnitCon l
        E.ListCon l       -> Core.ListCon l
        E.FunCon  l       -> Core.FunCon  l
        E.TupleCon l b n  -> Core.TupleCon l (fromHseBoxed b) n
        E.Cons l          -> Core.Cons l
        E.UnboxedSingleCon l  -> Core.UnboxedSingleCon l

fromHseQName :: Parsed_ E.QName -> Parsed Core.QName
fromHseQName qn = case qn of
        E.Qual    l mn n  -> Core.Qual    l (fromHseModuleName mn) (fromHseName n)
        E.UnQual  l    n  -> Core.UnQual  l (fromHseName n)
        E.Special l sc    -> Core.Special l (fromHseSpecialCon sc)

fromHseName :: Parsed_ E.Name -> Parsed Core.Name
fromHseName (E.Ident  l s) = Core.Ident  l s
fromHseName (E.Symbol l s) = Core.Symbol l s

fromHseIPName :: Parsed_ E.IPName -> Parsed ImplicitParams.IPName
fromHseIPName (E.IPDup l s) = ImplicitParams.IPDup l s
fromHseIPName (E.IPLin l s) = ImplicitParams.IPLin l s

fromHseQOp :: Parsed_ E.QOp -> Parsed Sugar.QOp
fromHseQOp (E.QVarOp l qn) = Sugar.QVarOp l (fromHseQName qn)
fromHseQOp (E.QConOp l qn) = Sugar.QConOp l (fromHseQName qn)

fromHseOp :: Parsed_ E.Op -> Parsed Core.Op
fromHseOp (E.VarOp l n) = Core.VarOp l (fromHseName n)
fromHseOp (E.ConOp l n) = Core.ConOp l (fromHseName n)

fromHseCName :: Parsed_ E.CName -> Parsed Core.CName
fromHseCName (E.VarName l n) = Core.VarName l (fromHseName n)
fromHseCName (E.ConName l n) = Core.ConName l (fromHseName n)


class Typeable t => HseModule t where
    fromHseModule :: Parsed_ E.Module -> ParseResult (Parsed t)

instance (HseDecl bind, HseDecl tydecl, HseDecl classreldecl, HseDecl declext, HseModulePragma mpragext)
  => HseModule (Core.Module bind tydecl classreldecl declext mpragext) where
    fromHseModule (E.Module l mmh ops iss dcls) =
        Core.Module l (fmap fromHseModuleHead mmh) <$> mapM fromHseModulePragma ops <*> pure (map fromHseImportDecl iss) <*> mapM fromHseDecl dcls
    fromHseModule m = fromHseFailed m


instance (HseDecl bind, HseDecl tydecl, HseDecl classreldecl, HseDecl declext, HseModulePragma mpragext, HseExp exp)
  => HseModule (HSX.Module bind tydecl classreldecl declext mpragext exp) where
   fromHseModule m = case m of
      E.XmlPage l mn os xn xas me es
        -> HSX.XmlPage l (fromHseModuleName mn) <$> mapM fromHseModulePragma os
                                                <*> pure (fromHseXName xn)
                                                <*> mapM fromHseXAttr xas
                                                <*> traverse fromHseExp me
                                                <*> mapM fromHseExp es
      E.XmlHybrid l mmh ops iss dcls xn xas me es
        -> HSX.XmlHybrid l (fmap fromHseModuleHead mmh) <$> mapM fromHseModulePragma ops
                                                        <*> pure (map fromHseImportDecl iss)
                                                        <*> mapM fromHseDecl dcls
                                                        <*> pure (fromHseXName xn)
                                                        <*> mapM fromHseXAttr xas
                                                        <*> traverse fromHseExp me
                                                        <*> mapM fromHseExp es
      _ -> fromHseFailed m

fromHseModuleHead :: Parsed_ E.ModuleHead -> Parsed Core.ModuleHead
fromHseModuleHead (E.ModuleHead l mn mwt mexpl) =
        Core.ModuleHead l (fromHseModuleName mn) (fmap fromHseWarningText mwt) (fmap fromHseExportSpecList mexpl)

fromHseExportSpecList :: Parsed_ E.ExportSpecList -> Parsed Core.ExportSpecList
fromHseExportSpecList (E.ExportSpecList l ess) = Core.ExportSpecList l (map fromHseExportSpec ess)

fromHseExportSpec :: Parsed_ E.ExportSpec -> Parsed Core.ExportSpec
fromHseExportSpec es = case es of
        E.EVar l qn            -> Core.EVar l (fromHseQName qn)
        E.EAbs l qn            -> Core.EAbs l (fromHseQName qn)
        E.EThingAll l qn       -> Core.EThingAll l (fromHseQName qn)
        E.EThingWith l qn cns  -> Core.EThingWith l (fromHseQName qn) (map fromHseCName cns)
        E.EModuleContents l mn -> Core.EModuleContents l (fromHseModuleName mn)

fromHseImportDecl :: Parsed_ E.ImportDecl -> Parsed Core.ImportDecl
fromHseImportDecl (E.ImportDecl l mn qual src pkg mmn mis) =
        Core.ImportDecl l (fromHseModuleName mn) qual src pkg (fmap fromHseModuleName mmn) (fmap fromHseImportSpecList mis)

fromHseImportSpecList :: Parsed_ E.ImportSpecList -> Parsed Core.ImportSpecList
fromHseImportSpecList (E.ImportSpecList l b iss) = Core.ImportSpecList l b (map fromHseImportSpec iss)

fromHseImportSpec :: Parsed_ E.ImportSpec -> Parsed Core.ImportSpec
fromHseImportSpec is = case is of
        E.IVar l n            -> Core.IVar l (fromHseName n)
        E.IAbs l n            -> Core.IAbs l (fromHseName n)
        E.IThingAll l n       -> Core.IThingAll l (fromHseName n)
        E.IThingWith l n cns  -> Core.IThingWith l (fromHseName n) (map fromHseCName cns)

fromHseAssoc :: Parsed_ E.Assoc -> Parsed_ Core.Assoc
fromHseAssoc (E.AssocNone  l) = Core.AssocNone  l
fromHseAssoc (E.AssocLeft  l) = Core.AssocLeft  l
fromHseAssoc (E.AssocRight l) = Core.AssocRight l

class Typeable t => HseDecl t where
  fromHseDecl :: Parsed_ E.Decl -> ParseResult (Parsed t)

instance HseDecl NoExts where
    fromHseDecl = fromHseFailed

instance (HseType ty, HseGuard guard, HseExp exp, HsePat pat, HseDecl bindext)
  => HseDecl (Core.Bind ty guard exp pat bindext) where
    fromHseDecl decl = case decl of
        E.FunBind   l ms          -> Core.FunBind l <$> mapM fromHseMatch ms
        E.PatBind   l p mt rhs bs -> Core.PatBind l <$> fromHsePat p <*> traverse fromHseType mt <*> fromHseRhs rhs <*> traverse fromHseBinds bs
        E.TypeSig   l ns t        -> Core.TypeSig l (map fromHseName ns) <$> fromHseType t
        E.InfixDecl l a k ops     -> pure $ Core.InfixDecl l (fromHseAssoc a) k (map fromHseOp ops)
        _                         -> fromHseFailed decl

instance (HseAsst asst, HseType ty, HseDecl tydeclext)
  => HseDecl (Core.TypeDecl asst ty tydeclext) where
    fromHseDecl decl = case decl of
        E.TypeDecl    l dh t -> Core.TypeDecl    l (fromHseDeclHead dh) <$> fromHseType t
        E.DefaultDecl l ts   -> Core.DefaultDecl l <$> mapM fromHseType ts
        E.DataDecl l dn mcx dh cds ders ->
            Core.DataDecl l (fromHseDataOrNew dn) <$> traverse fromHseContext mcx
                                                  <*> pure (fromHseDeclHead dh)
                                                  <*> mapM fromHseQualConDecl cds
                                                  <*> traverse fromHseDeriving ders
        _ -> fromHseFailed decl

instance (HseAsst asst, HseType ty)
  => HseDecl (GADTSyntax.TypeDecl asst ty ) where
    fromHseDecl decl = case decl of
        E.GDataDecl  l dn mcx dh mk gds ders ->
            GADTSyntax.GDataDecl l (fromHseDataOrNew dn) <$> traverse fromHseContext mcx <*> pure (fromHseDeclHead dh) <*> pure (fmap fromHseKind mk) <*> mapM fromHseGadtDecl gds <*> traverse fromHseDeriving ders
        _ -> fromHseFailed decl

instance (HseAsst asst, HseType ty, HseDecl bind, HseClassDecl classbodyext, HseInstDecl instbodyext, HseDecl classrelext)
    => HseDecl (Core.ClassRelatedDecl asst ty bind classbodyext instbodyext classrelext) where
    fromHseDecl decl = case decl of
          E.ClassDecl l mcx dh fds mcds -> Core.ClassDecl l <$> traverse fromHseContext mcx <*> pure (fromHseDeclHead dh) <*> pure (map fromHseFunDep fds) <*> traverse (mapM fromHseClassDecl) mcds
          E.InstDecl  l mcx ih mids     -> Core.InstDecl  l <$> traverse fromHseContext mcx <*> fromHseInstHead ih <*> traverse (mapM fromHseInstDecl) mids
          _                             -> Core.ClassRelatedExt <$> fromHseDecl decl

instance (HseDecl bind, HseDecl tydecl, HseDecl classreldecl, HseDecl declext)
    => HseDecl (Core.Decl bind tydecl classreldecl declext) where
    fromHseDecl d = (Core.TyDecl       <$> fromHseDecl d)
                <|> (Core.BindDecl     <$> fromHseDecl d)
                <|> (Core.ClassRelDecl <$> fromHseDecl d)
                <|> (Core.DeclExt      <$> fromHseDecl d)


instance (HseAsst asst, HseType ty) => HseDecl (StandaloneDeriving.ClassRelatedDecl asst ty) where
    fromHseDecl decl = case decl of
        E.DerivDecl l mcx ih -> StandaloneDeriving.DerivDecl l <$> traverse fromHseContext mcx <*> fromHseInstHead ih
        _ -> fromHseFailed decl

instance (HseAsst asst, HseType ty) => HseDecl (TypeFamilies.Decl asst ty) where
    fromHseDecl decl = case decl of
        E.TypeFamDecl  l dh mk     -> pure $ TypeFamilies.TypeFamDecl l (fromHseDeclHead dh) (fmap fromHseKind mk)
        E.DataFamDecl  l mcx dh mk        -> TypeFamilies.DataFamDecl l <$> traverse fromHseContext mcx <*> pure (fromHseDeclHead dh) <*> pure (fmap fromHseKind mk)
        E.TypeInsDecl  l t1 t2            -> TypeFamilies.TypeInsDecl l <$> fromHseType t1 <*> fromHseType t2
        E.DataInsDecl  l dn t cds ders    -> TypeFamilies.DataInsDecl l (fromHseDataOrNew dn) <$> fromHseType t <*> mapM fromHseQualConDecl cds <*> traverse fromHseDeriving ders
        E.GDataInsDecl l dn t mk gds ders -> TypeFamilies.GDataInsDecl l (fromHseDataOrNew dn) <$> fromHseType t <*> pure (fmap fromHseKind mk) <*> mapM fromHseGadtDecl gds <*> traverse fromHseDeriving ders
        _ -> fromHseFailed decl

instance HseExp exp => HseDecl (TH.Decl exp) where
    fromHseDecl decl = case decl of
        E.SpliceDecl l sp -> TH.SpliceDecl l <$> fromHseExp sp
        _ -> fromHseFailed decl

instance HseType ty => HseDecl (FFI.Decl ty) where
    fromHseDecl decl = case decl of
        E.ForImp l cc msf s n t -> FFI.ForImp l (fromHseCallConv cc) (fmap fromHseSafety msf) s (fromHseName n) <$> fromHseType t
        E.ForExp l cc     s n t -> FFI.ForExp l (fromHseCallConv cc)                          s (fromHseName n) <$> fromHseType t
        _ -> fromHseFailed decl

instance (HseType ty, HseExp exp) => HseDecl (Pragmas.Decl_RulePragma ty exp) where
    fromHseDecl decl = case decl of
        E.RulePragmaDecl l rs -> Pragmas.RulePragmaDecl l <$> mapM fromHseRule rs
        _ -> fromHseFailed decl

instance HseDecl Pragmas.Decl_DeprPragma where
    fromHseDecl decl = case decl of
        E.DeprPragmaDecl l nss -> pure $ Pragmas.DeprPragmaDecl l (map wp nss)
        _ -> fromHseFailed decl
      where wp (ns, s) = (map fromHseName ns, s)

instance HseDecl Pragmas.Decl_WarnPragma where
    fromHseDecl decl = case decl of
        E.WarnPragmaDecl l nss -> pure $ Pragmas.WarnPragmaDecl l (map wp nss)
        _ -> fromHseFailed decl
      where wp (ns, s) = (map fromHseName ns, s)

instance HseDecl Pragmas.Decl_InlinePragma where
    fromHseDecl decl = case decl of
        E.InlineSig l b mact qn -> pure $ Pragmas.InlineSig l b (fmap fromHseActivation mact) (fromHseQName qn)
        _ -> fromHseFailed decl

instance HseDecl Pragmas.Decl_InlineConPragma where
    fromHseDecl decl = case decl of
        E.InlineConlikeSig l mact qn -> pure $ Pragmas.InlineConlikeSig l (fmap fromHseActivation mact) (fromHseQName qn)
        _ -> fromHseFailed decl

instance HseType ty => HseDecl (Pragmas.Decl_SpecPragma ty) where
    fromHseDecl decl = case decl of
        E.SpecSig l mact qn ts -> Pragmas.SpecSig l (fmap fromHseActivation mact) (fromHseQName qn) <$> mapM fromHseType ts
        _ -> fromHseFailed decl

instance HseType ty => HseDecl (Pragmas.Decl_SpecInlinePragma ty) where
    fromHseDecl decl = case decl of
        E.SpecInlineSig l b mact qn ts -> Pragmas.SpecInlineSig l b (fmap fromHseActivation mact) (fromHseQName qn) <$> mapM fromHseType ts
        _ -> fromHseFailed decl

instance (HseAsst asst, HseType ty) => HseDecl (Pragmas.Decl_SpecInstPragma asst ty) where
    fromHseDecl decl = case decl of
        E.InstSig l mcx ih -> Pragmas.InstSig l <$> traverse fromHseContext mcx <*> fromHseInstHead ih
        _ -> fromHseFailed decl

instance HseExp exp => HseDecl (Pragmas.Decl_AnnPragma exp) where
    fromHseDecl decl = case decl of
        E.AnnPragma l ann -> Pragmas.AnnPragma l <$> fromHseAnnotation ann
        _ -> fromHseFailed decl

fromHseAnnotation :: HseExp exp => Parsed_ E.Annotation -> ParseResult (Parsed (Pragmas.Annotation exp))
fromHseAnnotation (E.Ann     l n e) = Pragmas.Ann     l (fromHseName n) <$> fromHseExp e
fromHseAnnotation (E.TypeAnn l n e) = Pragmas.TypeAnn l (fromHseName n) <$> fromHseExp e
fromHseAnnotation (E.ModuleAnn l e) = Pragmas.ModuleAnn l <$> fromHseExp e

fromHseDataOrNew :: Parsed_ E.DataOrNew -> Parsed_ Core.DataOrNew
fromHseDataOrNew (E.DataType l) = Core.DataType l
fromHseDataOrNew (E.NewType  l) = Core.NewType  l

fromHseDeclHead :: Parsed_ E.DeclHead -> Parsed Core.DeclHead
fromHseDeclHead (E.DHead l n tvs)       =  Core.DHead   l (fromHseName n) (map fromHseTyVarBind tvs)
fromHseDeclHead (E.DHInfix l tva n tvb) =  Core.DHInfix l (fromHseTyVarBind tva) (fromHseName n) (fromHseTyVarBind tvb)
fromHseDeclHead (E.DHParen l dh)        =  Core.DHParen l (fromHseDeclHead dh)

fromHseInstHead :: HseType ty => Parsed_ E.InstHead -> ParseResult (Parsed (Core.InstHead ty))
fromHseInstHead (E.IHead l qn ts)       = Core.IHead l (fromHseQName qn) <$> mapM fromHseType ts
fromHseInstHead (E.IHInfix l ta qn tb)  = Core.IHInfix l <$> fromHseType ta <*> pure (fromHseQName qn) <*> fromHseType tb
fromHseInstHead (E.IHParen l ih)        = Core.IHParen l <$> fromHseInstHead ih

fromHseDeriving :: HseType ty => Parsed_ E.Deriving -> ParseResult (Parsed (Core.Deriving ty))
fromHseDeriving (E.Deriving l ihs) = Core.Deriving l <$> mapM fromHseInstHead ihs


class Typeable t => HseBinds t where
    fromHseBinds :: Parsed_ E.Binds -> ParseResult (Parsed t)

instance (HseType ty, HseGuard guard, HseExp exp, HsePat pat, HseDecl bindext)
  => HseBinds (Core.Binds ty guard exp pat bindext) where
    fromHseBinds (E.BDecls l decls) = Core.BDecls l <$> mapM fromHseDecl decls
    fromHseBinds bs = fromHseFailed bs

instance HseExp exp => HseBinds (ImplicitParams.Binds exp) where
    fromHseBinds (E.IPBinds l ibs) = ImplicitParams.IPBinds l <$> mapM fromHseIPBind ibs
    fromHseBinds bs = fromHseFailed bs

fromHseIPBind :: HseExp exp => Parsed_ E.IPBind -> ParseResult (Parsed (ImplicitParams.IPBind exp))
fromHseIPBind (E.IPBind l ipn e) = ImplicitParams.IPBind l (fromHseIPName ipn) <$> fromHseExp e

fromHseMatch :: (HseType ty, HseGuard guard, HseExp exp, HsePat pat, HseDecl bindext)
             => Parsed_ E.Match -> ParseResult (Parsed (Core.Match ty guard exp pat bindext))
fromHseMatch m = case m of
   E.Match l n ps rhs bs
     -> Core.Match l (fromHseName n) <$> mapM fromHsePat ps <*> fromHseRhs rhs <*> traverse fromHseBinds bs
   E.InfixMatch l a n ps rhs bs
     -> Core.InfixMatch l <$> fromHsePat a <*> pure (fromHseName n) <*> mapM fromHsePat ps <*> fromHseRhs rhs <*> traverse fromHseBinds bs

fromHseQualConDecl :: (HseAsst asst, HseType ty)
                   => Parsed_ E.QualConDecl -> ParseResult (Parsed (Core.QualConDecl asst ty))
fromHseQualConDecl (E.QualConDecl l mtvs mcx cd) = Core.QualConDecl l (fmap (map fromHseTyVarBind) mtvs) <$> traverse fromHseContext mcx <*> fromHseConDecl cd

fromHseConDecl :: HseType ty => Parsed_ E.ConDecl -> ParseResult (Parsed (Core.ConDecl ty))
fromHseConDecl (E.ConDecl l n bts) = Core.ConDecl l (fromHseName n) <$> mapM fromHseBangType bts
fromHseConDecl (E.InfixConDecl l ta n tb) = Core.InfixConDecl l <$> fromHseBangType ta <*> pure (fromHseName n) <*> fromHseBangType tb
fromHseConDecl (E.RecDecl l n fds) = Core.RecDecl l (fromHseName n) <$> mapM fromHseFieldDecl fds

fromHseFieldDecl :: HseType ty => Parsed_ E.FieldDecl -> ParseResult (Parsed (Core.FieldDecl ty))
fromHseFieldDecl (E.FieldDecl l ns t) = Core.FieldDecl l (map fromHseName ns) <$> fromHseBangType t

fromHseGadtDecl :: HseType ty => Parsed_ E.GadtDecl -> ParseResult (Parsed (GADTSyntax.GadtDecl ty))
fromHseGadtDecl (E.GadtDecl l n t) = GADTSyntax.GadtDecl l (fromHseName n) <$> fromHseType t

class Typeable t => HseClassDecl t where
    fromHseClassDecl :: Parsed_ E.ClassDecl -> ParseResult (Parsed t)

instance HseClassDecl NoExts where
  fromHseClassDecl = fromHseFailed

instance (HseDecl bind, HseClassDecl classbodyext)
    => HseClassDecl (Core.ClassBody bind classbodyext) where
    fromHseClassDecl (E.ClsDecl l d) = Core.ClsDecl l <$> fromHseDecl d
    fromHseClassDecl decl = Core.ClassBodyExt <$> fromHseClassDecl decl


instance (HseAsst asst, HseType ty)
    => HseClassDecl (TypeFamilies.ClassBody asst ty) where
    fromHseClassDecl decl = case decl of
        E.ClsDataFam l mcx dh mk -> TypeFamilies.ClsDataFam l <$> traverse fromHseContext mcx <*> pure (fromHseDeclHead dh) <*> pure (fmap fromHseKind mk)
        E.ClsTyFam   l     dh mk -> pure $ TypeFamilies.ClsTyFam   l (fromHseDeclHead dh) (fmap fromHseKind mk)
        E.ClsTyDef   l t1 t2     -> TypeFamilies.ClsTyDef   l <$> fromHseType t1 <*> fromHseType t2
        _ -> fromHseFailed decl


class Typeable t => HseInstDecl t where
    fromHseInstDecl :: Parsed_ E.InstDecl -> ParseResult (Parsed t)

instance HseInstDecl NoExts where
  fromHseInstDecl = fromHseFailed

instance (HseDecl bind, HseInstDecl instbodyext)
    => HseInstDecl (Core.InstBody bind instbodyext) where
      fromHseInstDecl idecl = case idecl of
          E.InsDecl l d -> Core.InsDecl l <$> fromHseDecl d
          _ -> Core.InsBodyExt <$> fromHseInstDecl idecl

instance (HseAsst asst, HseType ty)
    => HseInstDecl (TypeFamilies.InstBody asst ty) where
      fromHseInstDecl idecl = case idecl of
        E.InsType   l t1 t2 -> TypeFamilies.InsType l <$> fromHseType t1 <*> fromHseType t2
        E.InsData   l dn t  cds ders
            -> TypeFamilies.InsData  l (fromHseDataOrNew dn) <$> fromHseType t <*> mapM fromHseQualConDecl cds <*> traverse fromHseDeriving ders
        E.InsGData  l dn t mk gds ders
            -> TypeFamilies.InsGData l (fromHseDataOrNew dn) <$> fromHseType t <*> pure (fmap fromHseKind mk) <*> mapM fromHseGadtDecl gds <*> traverse fromHseDeriving ders
        _   -> fromHseFailed idecl


fromHseBangType :: HseType ty => Parsed_ E.BangType -> ParseResult (Parsed (Core.BangType ty))
fromHseBangType (E.BangedTy   l t) = Core.BangedTy l <$> fromHseType t
fromHseBangType (E.UnBangedTy l t) = Core.UnBangedTy l <$> fromHseType t
fromHseBangType (E.UnpackedTy l t) = Core.UnpackedTy l <$> fromHseType t

fromHseRhs :: (HseGuard guard, HseExp exp)
           =>  Parsed_ E.Rhs -> ParseResult (Parsed (Core.Rhs guard exp))
fromHseRhs (E.UnGuardedRhs l e)     = Core.UnGuardedRhs l <$> fromHseExp e
fromHseRhs (E.GuardedRhss  l grhss) = Core.GuardedRhss  l <$> mapM fromHseGuardedRhs grhss

fromHseGuardedRhs :: (HseGuard guard, HseExp exp)
                  =>  Parsed_ E.GuardedRhs -> ParseResult (Parsed (Core.GuardedRhs guard exp))
fromHseGuardedRhs (E.GuardedRhs l ss e) = Core.GuardedRhs l <$> fromHseGuard l ss <*> fromHseExp e

class Typeable t => HseType t where
    fromHseType :: Parsed_ E.Type -> ParseResult (Parsed t)

instance HseType NoExts where
    fromHseType = fromHseFailed

instance HseType tyext => HseType (Core.Type tyext) where
    fromHseType t = case t of
      E.TyFun   l t1 t2 -> Core.TyFun l <$> fromHseType t1 <*> fromHseType t2
      E.TyApp   l t1 t2 -> Core.TyApp l <$> fromHseType t1 <*> fromHseType t2
      E.TyVar   l n     -> pure $ Core.TyVar l (fromHseName n)
      E.TyCon   l qn    -> pure $ Core.TyCon l (fromHseQName qn)
      _                 -> Core.TyExt <$> fromHseType t

instance HseType ty => HseType (Sugar.Type ty) where
    fromHseType t = case t of
      E.TyTuple l b ts     -> Sugar.TyTuple l (fromHseBoxed b) <$> mapM fromHseType ts
      E.TyList  l ty       -> Sugar.TyList  l <$> fromHseType ty
      E.TyParen l ty       -> Sugar.TyParen l <$> fromHseType ty
      E.TyInfix l ta qn tb -> Sugar.TyInfix l <$> fromHseType ta <*> pure (fromHseQName qn) <*> fromHseType tb
      E.TyKind  l ty k     -> Sugar.TyKind  l <$> fromHseType ty <*> pure (fromHseKind k)
      _                    -> fromHseFailed t

instance (HseAsst asst, HseType ty) => HseType (Core.QualType asst ty) where
    fromHseType (E.TyForall l mtvs mcx t) = Core.TyForall l (fmap (map fromHseTyVarBind) mtvs) <$> traverse fromHseContext mcx <*> fromHseType t
    fromHseType t = fromHseFailed t

fromHseBoxed :: E.Boxed -> Core.Boxed
fromHseBoxed E.Boxed   = Core.Boxed
fromHseBoxed E.Unboxed = Core.Unboxed

fromHseTyVarBind :: Parsed_ E.TyVarBind -> Parsed Core.TyVarBind
fromHseTyVarBind (E.KindedVar   l n k) = Core.KindedVar   l (fromHseName n) (fromHseKind k)
fromHseTyVarBind (E.UnkindedVar l n)   = Core.UnkindedVar l (fromHseName n)

fromHseKind :: Parsed_ E.Kind -> Parsed Core.Kind
fromHseKind (E.KindStar  l)       = Core.KindStar l
fromHseKind (E.KindBang  l)       = Core.KindBang l
fromHseKind (E.KindFn    l k1 k2) = Core.KindFn l (fromHseKind k1) (fromHseKind k2)
fromHseKind (E.KindParen l k)     = Core.KindParen l (fromHseKind k)
fromHseKind (E.KindVar   l n)     = Core.KindVar l (fromHseName n)

fromHseFunDep :: Parsed_ E.FunDep -> Parsed Core.FunDep
fromHseFunDep (E.FunDep l ns1 ns2) = Core.FunDep l (map fromHseName ns1) (map fromHseName ns2)

fromHseContext :: HseAsst asst => Parsed_ E.Context -> ParseResult (Parsed (Core.Context asst))
fromHseContext (E.CxSingle l asst) = Core.CxSingle l <$> fromHseAsst asst
fromHseContext (E.CxTuple l assts) = Core.CxTuple  l <$> mapM fromHseAsst assts
fromHseContext (E.CxParen l ctxt)  = Core.CxParen  l <$> fromHseContext ctxt
fromHseContext (E.CxEmpty l)       = pure $ Core.CxEmpty  l

class Typeable t => HseAsst t where
   fromHseAsst :: Parsed_ E.Asst -> ParseResult (Parsed t)

instance HseAsst NoExts where
   fromHseAsst = fromHseFailed

instance (HseType ty, HseAsst asstext) => HseAsst (Core.Asst ty asstext) where
    fromHseAsst (E.ClassA l qn ts) = Core.ClassA l (fromHseQName qn) <$> mapM fromHseType ts
    fromHseAsst a = Core.AsstExt <$> fromHseAsst a

instance HseType ty => HseAsst (MultiParamTypeClasses.AsstInfix ty) where
    fromHseAsst (E.InfixA l ta qn tb) = MultiParamTypeClasses.InfixA l <$> fromHseType ta <*> pure (fromHseQName qn) <*> fromHseType tb
    fromHseAsst a = fromHseFailed a

instance HseType ty => HseAsst (ImplicitParams.Asst ty) where
    fromHseAsst (E.IParam l ipn t) = ImplicitParams.IParam l (fromHseIPName ipn) <$> fromHseType t
    fromHseAsst a = fromHseFailed a

instance HseType ty => HseAsst (TypeFamilies.AsstEq ty) where
    fromHseAsst (E.EqualP l t1 t2) = TypeFamilies.EqualP l <$> fromHseType t1 <*> fromHseType t2
    fromHseAsst a = fromHseFailed a



class Typeable t => HseLiteral t where
  fromHseLiteral :: Parsed_ E.Literal -> ParseResult (Parsed_ t)

instance HseLiteral Core.Literal where
    fromHseLiteral lit = case lit of
        E.Char       l c rw -> pure $ Core.Char       l c rw
        E.String     l s rw -> pure $ Core.String     l s rw
        E.Int        l i rw -> pure $ Core.Int        l i rw
        E.Frac       l r rw -> pure $ Core.Frac       l r rw
        E.PrimInt    l i rw -> pure $ Core.PrimInt    l i rw
        E.PrimWord   l i rw -> pure $ Core.PrimWord   l i rw
        E.PrimFloat  l r rw -> pure $ Core.PrimFloat  l r rw
        E.PrimDouble l r rw -> pure $ Core.PrimDouble l r rw
        E.PrimChar   l c rw -> pure $ Core.PrimChar   l c rw
        E.PrimString l s rw -> pure $ Core.PrimString l s rw

class Typeable t => HseExp t where
  fromHseExp :: Parsed_ E.Exp -> ParseResult (Parsed t)

instance HseExp NoExts where
  fromHseExp e = fromHseFailed e

instance (HseBinds binds, HsePat pat, HseLiteral lit, HseExp expext)
   => HseExp (Core.Exp binds pat lit expext) where
    fromHseExp e = case e of
        E.Var l qn       -> pure $ Core.Var l (fromHseQName qn)
        E.Con l qn       -> pure $ Core.Con l (fromHseQName qn)
        E.Lit l lit      -> Core.Lit l <$> fromHseLiteral lit
        E.App l e1 e2    -> Core.App l <$> fromHseExp e1 <*> fromHseExp e2
        E.Lambda l ps ex -> Core.Lambda l <$> mapM fromHsePat ps <*> fromHseExp ex
        E.Let l bs ex    -> Core.Let l <$> fromHseBinds bs <*> fromHseExp ex
        E.Case l ex alts
           | alts'@ParseOk{} <- mapM simpleAlt alts
                         -> Core.Case l <$> fromHseExp ex <*> alts'
        _ -> Core.ExpExt <$> fromHseExp e
      where
        simpleAlt a = case a of
          E.Alt l p (E.UnGuardedAlt _ ex) Nothing
             ->  Core.Alt l <$> fromHsePat p <*> fromHseExp ex
          _ -> fromHseFailed a


instance (HseBinds binds, HseType ty, HseGuard guard, HsePat pat, HseStmt stmtext,  HseExp exp)
   => HseExp (Sugar.Exp binds ty guard pat stmtext exp) where
    fromHseExp e = case e of
        E.Case l ex alts        -> Sugar.CaseAlt l <$> fromHseExp ex <*> mapM fromHseAlt alts
        E.InfixApp l e1 qop e2  -> Sugar.InfixApp l <$> fromHseExp e1 <*> pure (fromHseQOp qop) <*> fromHseExp e2
        E.NegApp   l ex         -> Sugar.NegApp l <$> fromHseExp ex
        E.If l ec et ee         -> Sugar.If l <$> fromHseExp ec <*> fromHseExp et <*> fromHseExp ee
        E.Do l ss               -> Sugar.Do l <$> mapM fromHseStmt ss
        E.Tuple l bx es         -> Sugar.Tuple l (fromHseBoxed bx) <$> mapM fromHseExp es
        E.TupleSection l bx mes -> Sugar.TupleSection l (fromHseBoxed bx) <$> mapM (traverse fromHseExp) mes
        E.List l es             -> Sugar.List l <$> mapM fromHseExp es
        E.Paren l ex            -> Sugar.Paren l <$> fromHseExp ex
        E.LeftSection l ex qop  -> Sugar.LeftSection l <$> fromHseExp ex <*> pure (fromHseQOp qop)
        E.RightSection l qop ex -> Sugar.RightSection l (fromHseQOp qop) <$> fromHseExp ex
        E.RecConstr l qn fups   -> Sugar.RecConstr l (fromHseQName qn) <$> mapM fromHseFieldUpdate fups
        E.RecUpdate l ex fups   -> Sugar.RecUpdate l <$> fromHseExp ex <*> mapM fromHseFieldUpdate fups
        E.EnumFrom l ex         -> Sugar.EnumFrom     l <$> fromHseExp ex
        E.EnumFromTo l ef et    -> Sugar.EnumFromTo   l <$> fromHseExp ef <*> fromHseExp et
        E.EnumFromThen l ef et  -> Sugar.EnumFromThen l <$> fromHseExp ef <*> fromHseExp et
        E.EnumFromThenTo l ef eth eto -> Sugar.EnumFromThenTo l <$> fromHseExp ef <*> fromHseExp eth <*> fromHseExp eto
        E.ListComp l ex qss     -> Sugar.ListComp l <$> fromHseExp ex <*> mapM fromHseQualStmt qss
        E.ExpTypeSig l ex t     -> Sugar.ExpTypeSig l <$> fromHseExp ex <*> fromHseType t
        _ -> fromHseFailed e

instance (HseStmt stmt, HseExp exp) => HseExp (TransformListComp.Exp stmt exp) where
    fromHseExp (E.ListComp l ex qss) = TransformListComp.EListComp l <$> fromHseExp ex <*> mapM fromHseQualStmt qss
    fromHseExp e  = fromHseFailed e


instance HseExp ImplicitParams.Exp where
    fromHseExp (E.IPVar l ipn) = pure $ ImplicitParams.IPVar l (fromHseIPName ipn)
    fromHseExp e  = fromHseFailed e

instance HseStmt stmt => HseExp (RecursiveDo.Stmt stmt) where
    fromHseExp (E.MDo l ss) = RecursiveDo.MDo l <$> mapM fromHseStmt ss
    fromHseExp e  = fromHseFailed e

instance (HseQualStmt qstmt, HseExp exp) => HseExp (ParallelListComp.Exp qstmt exp) where
    fromHseExp (E.ParComp l e qsss) = ParallelListComp.ParComp l <$> fromHseExp e <*> mapM (mapM fromHseQualStmt) qsss
    fromHseExp e  = fromHseFailed e

instance (HseDecl decl, HseType ty, HseExp exp, HsePat pat)
  => HseExp (TH.Exp decl ty exp pat) where
    fromHseExp e = case e of
        E.VarQuote l qn      -> pure $ TH.VarQuote l (fromHseQName qn)
        E.TypQuote l qn      -> pure $ TH.TypQuote l (fromHseQName qn)
        E.BracketExp l br    -> TH.BracketExp l <$> fromHseBracket br
        E.SpliceExp l sp     -> TH.SpliceExp  l <$> fromHseSplice sp
        E.QuasiQuote l sn se -> pure $ TH.QuasiQuote l sn se
        _ -> fromHseFailed e

instance HseExp exp => HseExp (Pragmas.Exp_CorePragma exp) where
    fromHseExp (E.CorePragma l s e) = Pragmas.CorePragma l s <$> fromHseExp e
    fromHseExp e  = fromHseFailed e

instance HseExp exp => HseExp (Pragmas.Exp_SCCPragma exp) where
    fromHseExp (E.SCCPragma l s e) = Pragmas.SCCPragma l s <$> fromHseExp e
    fromHseExp e  = fromHseFailed e

instance HseExp exp => HseExp (Pragmas.Exp_GenPragma exp) where
    fromHseExp (E.GenPragma l s n12 n34 e) = Pragmas.GenPragma l s n12 n34 <$> fromHseExp e
    fromHseExp e  = fromHseFailed e

instance (HseExp exp, HsePat pat) => HseExp (Arrows.Exp exp pat) where
    fromHseExp e = case e of
        E.Proc            l p ex  -> Arrows.Proc            l <$> fromHsePat p  <*> fromHseExp ex
        E.LeftArrApp      l e1 e2 -> Arrows.LeftArrApp      l <$> fromHseExp e1 <*> fromHseExp e2
        E.RightArrApp     l e1 e2 -> Arrows.RightArrApp     l <$> fromHseExp e1 <*> fromHseExp e2
        E.LeftArrHighApp  l e1 e2 -> Arrows.LeftArrHighApp  l <$> fromHseExp e1 <*> fromHseExp e2
        E.RightArrHighApp l e1 e2 -> Arrows.RightArrHighApp l <$> fromHseExp e1 <*> fromHseExp e2
        _ -> fromHseFailed e

instance HseExp exp => HseExp (HSX.Exp exp) where
    fromHseExp e = case e of
        E.XTag  l xn xas me es -> HSX.XTag  l (fromHseXName xn) <$> mapM fromHseXAttr xas <*> traverse fromHseExp me <*> mapM fromHseExp es
        E.XETag l xn xas me    -> HSX.XETag l (fromHseXName xn) <$> mapM fromHseXAttr xas <*> traverse fromHseExp me
        E.XPcdata l s          -> pure $ HSX.XPcdata l s
        E.XExpTag l ex         -> HSX.XExpTag l <$> fromHseExp ex
        E.XChildTag l es       -> HSX.XChildTag l <$> mapM fromHseExp es
        _ -> fromHseFailed e


fromHseXName :: Parsed_ E.XName -> Parsed HSX.XName
fromHseXName (E.XName l s)        = HSX.XName l s
fromHseXName (E.XDomName l sd sn) = HSX.XDomName l sd sn

fromHseXAttr :: HseExp exp => Parsed_ E.XAttr -> ParseResult (Parsed (HSX.XAttr exp))
fromHseXAttr (E.XAttr l xn e) = HSX.XAttr l (fromHseXName xn) <$> fromHseExp e

fromHseBracket :: (HseDecl decl, HseType ty, HseExp exp, HsePat pat)
               => Parsed_ E.Bracket -> ParseResult (Parsed (TH.Bracket decl ty exp pat))
fromHseBracket (E.ExpBracket  l e)  = TH.ExpBracket  l <$> fromHseExp e
fromHseBracket (E.PatBracket  l p)  = TH.PatBracket  l <$> fromHsePat p
fromHseBracket (E.TypeBracket l t)  = TH.TypeBracket l <$> fromHseType t
fromHseBracket (E.DeclBracket l ds) = TH.DeclBracket l <$> mapM fromHseDecl ds

fromHseSplice :: HseExp exp => Parsed_ E.Splice -> ParseResult (Parsed (TH.Splice exp))
fromHseSplice (E.IdSplice    l s) = pure $ TH.IdSplice l s
fromHseSplice (E.ParenSplice l e) = TH.ParenSplice l <$> fromHseExp e

fromHseSafety :: Parsed_ E.Safety -> Parsed_ FFI.Safety
fromHseSafety (E.PlayRisky l)         = FFI.PlayRisky l
fromHseSafety (E.PlaySafe l b)        = FFI.PlaySafe l b
fromHseSafety (E.PlayInterruptible l) = FFI.PlayInterruptible l

fromHseCallConv :: Parsed_ E.CallConv -> Parsed_ FFI.CallConv
fromHseCallConv (E.StdCall l)   = FFI.StdCall l
fromHseCallConv (E.CCall l)     = FFI.CCall l
fromHseCallConv (E.CPlusPlus l) = FFI.CPlusPlus l
fromHseCallConv (E.DotNet l)    = FFI.DotNet l
fromHseCallConv (E.Jvm l)       = FFI.Jvm l
fromHseCallConv (E.Js l)        = FFI.Js l
fromHseCallConv (E.CApi l)      = FFI.CApi l

class Typeable t => HseModulePragma t where
    fromHseModulePragma :: Parsed_ E.ModulePragma -> ParseResult (Parsed t)

instance HseModulePragma NoExts where
    fromHseModulePragma = fromHseFailed

instance HseModulePragma mpragext => HseModulePragma (Core.ModulePragma mpragext) where
    fromHseModulePragma (E.LanguagePragma l ns)   = pure $ Core.LanguagePragma l (map fromHseName ns)
    fromHseModulePragma (E.OptionsPragma  l mt s) = pure $ Core.OptionsPragma  l (fmap fromHseTool mt) s
    fromHseModulePragma pragma = Core.ModulePragmaExt <$> fromHseModulePragma pragma


fromHseTool :: E.Tool -> Core.Tool
fromHseTool t = case t of
  E.GHC -> Core.GHC
  E.HUGS -> Core.HUGS
  E.NHC98 -> Core.NHC98
  E.YHC -> Core.YHC
  E.HADDOCK -> Core.HADDOCK
  E.UnknownTool s -> Core.UnknownTool s

instance HseExp exp => HseModulePragma (Pragmas.ModulePragma_Ann exp) where
    fromHseModulePragma (E.AnnModulePragma l ann) = Pragmas.AnnModulePragma l <$> fromHseAnnotation ann
    fromHseModulePragma pragma = fromHseFailed pragma

fromHseActivation :: Parsed_ E.Activation -> Parsed_ Pragmas.Activation
fromHseActivation (E.ActiveFrom   l k) = Pragmas.ActiveFrom l k
fromHseActivation (E.ActiveUntil  l k) = Pragmas.ActiveUntil l k

fromHseRule :: (HseType ty, HseExp exp) => Parsed_ E.Rule -> ParseResult (Parsed (Pragmas.Rule ty exp))
fromHseRule (E.Rule l s mact mrvs e1 e2) =
        Pragmas.Rule l s (fmap fromHseActivation mact) <$> traverse (mapM fromHseRuleVar) mrvs <*> fromHseExp e1 <*> fromHseExp e2

fromHseRuleVar :: HseType ty => Parsed_ E.RuleVar -> ParseResult (Parsed (Pragmas.RuleVar ty))
fromHseRuleVar (E.RuleVar l n) = pure $ Pragmas.RuleVar l (fromHseName n)
fromHseRuleVar (E.TypedRuleVar l n t) = Pragmas.TypedRuleVar l (fromHseName n) <$> fromHseType t

fromHseWarningText :: Parsed_ E.WarningText -> Parsed_ Core.WarningText
fromHseWarningText (E.DeprText l s) = Core.DeprText l s
fromHseWarningText (E.WarnText l s) = Core.WarnText l s

class Typeable t => HsePat t where
    fromHsePat :: Parsed_ E.Pat -> ParseResult (Parsed t)

instance HsePat NoExts where
    fromHsePat = fromHseFailed

instance HsePat patext => HsePat (Core.Pat patext) where
    fromHsePat p = case p of
        E.PVar l n     -> pure $ Core.PVar l (fromHseName n)
        E.PApp l qn ps -> Core.PApp l (fromHseQName qn) <$> mapM fromHsePat ps
        E.PWildCard l  -> pure $ Core.PWildCard l
        _              -> Core.PExt <$> fromHsePat p

instance (HsePat pat, HseLiteral lit) => HsePat (Sugar.Pat lit pat) where
    fromHsePat p = case p of
        E.PLit l lit     -> Sugar.PLit l <$> fromHseLiteral lit
        E.PNeg l pat     -> Sugar.PNeg l <$> fromHsePat pat
        E.PTuple l bx ps -> Sugar.PTuple l (fromHseBoxed bx) <$> mapM fromHsePat ps
        E.PList l ps     -> Sugar.PList l  <$> mapM fromHsePat ps
        E.PParen l pat   -> Sugar.PParen l <$> fromHsePat pat
        E.PRec l qn pfs  -> Sugar.PRec l (fromHseQName qn)  <$> mapM fromHsePatField pfs
        E.PAsPat l n pat -> Sugar.PAsPat  l (fromHseName n) <$> fromHsePat pat
        E.PIrrPat l pat  -> Sugar.PIrrPat l <$> fromHsePat pat
        E.PInfixApp l pa qn pb
              -> Sugar.PInfixApp l <$> fromHsePat pa <*> pure (fromHseQName qn) <*> fromHsePat pb
        _ -> fromHseFailed p

instance HsePat pat => HsePat (Patterns.BangPat pat) where
    fromHsePat p = case p of
        E.PBangPat l pat -> Patterns.PBangPat l <$> fromHsePat pat
        _  -> fromHseFailed p

instance (HseType ty, HsePat pat) => HsePat (Patterns.PatTySig ty pat) where
    fromHsePat p = case p of
        E.PatTypeSig l pat t -> Patterns.PatTypeSig l <$> fromHsePat pat <*> fromHseType t
        _  -> fromHseFailed p

instance HsePat Patterns.NPlusKPat where
    fromHsePat p = case p of
        E.PNPlusK l n k -> pure $ Patterns.PNPlusK l (fromHseName n) k
        _  -> fromHseFailed p

instance HseType ty => HsePat (Patterns.PatExplTyArg ty) where
    fromHsePat p = case p of
        E.PExplTypeArg l qn t   -> Patterns.PExplTypeArg l (fromHseQName qn) <$> fromHseType t
        _  -> fromHseFailed p

instance (HseExp exp, HsePat pat) => HsePat (ViewPatterns.Pat exp pat) where
    fromHsePat p = case p of
        E.PViewPat l e pat -> ViewPatterns.PViewPat l <$> fromHseExp e <*> fromHsePat pat
        _  -> fromHseFailed p

instance (HseStmt stmt, HsePat pat) => HsePat (HaRP.Pat_RegList stmt pat) where
    fromHsePat p = case p of
        E.PRPat l rps -> HaRP.PRPat l <$> mapM fromHseRPat rps
        _  -> fromHseFailed p

instance (HseStmt stmt, HsePat pat) => HsePat (HaRP.Pat_XRegList stmt pat) where
    fromHsePat p = case p of
        E.PXRPats l rps -> HaRP.PXRPats l <$> mapM fromHseRPat rps
        _  -> fromHseFailed p

instance HsePat pat => HsePat (HSX.Pat pat) where
    fromHsePat p = case p of
        E.PXTag l xn pxas mp ps -> HSX.PXTag  l (fromHseXName xn) <$> mapM fromHsePXAttr pxas <*> traverse fromHsePat mp <*> mapM fromHsePat ps
        E.PXETag l xn pxas mp   -> HSX.PXETag l (fromHseXName xn) <$> mapM fromHsePXAttr pxas <*> traverse fromHsePat mp
        E.PXPcdata l s          -> pure $ HSX.PXPcdata l s
        E.PXPatTag l pat        -> HSX.PXPatTag l <$> fromHsePat pat
        _  -> fromHseFailed p

instance HsePat TH.Pat where
    fromHsePat p = case p of
        E.PQuasiQuote l sn st -> pure $ TH.PQuasiQuote l sn st
        _  -> fromHseFailed p



fromHsePXAttr :: HsePat pat => Parsed_ E.PXAttr -> ParseResult (Parsed (HSX.PXAttr pat))
fromHsePXAttr (E.PXAttr l xn p) = HSX.PXAttr l (fromHseXName xn) <$> fromHsePat p

fromHseRPatOp :: Parsed_ E.RPatOp -> Parsed_ HaRP.RPatOp
fromHseRPatOp (E.RPStar  l) = HaRP.RPStar l
fromHseRPatOp (E.RPStarG l) = HaRP.RPStarG l
fromHseRPatOp (E.RPPlus  l) = HaRP.RPPlus l
fromHseRPatOp (E.RPPlusG l) = HaRP.RPPlusG l
fromHseRPatOp (E.RPOpt   l) = HaRP.RPOpt l
fromHseRPatOp (E.RPOptG  l) = HaRP.RPOptG l

fromHseRPat :: (HseStmt stmt, HsePat pat)
            => Parsed_ E.RPat -> ParseResult (Parsed (HaRP.RPat stmt pat))
fromHseRPat rpat = case rpat of
      E.RPOp l rp rop      -> HaRP.RPOp l <$> fromHseRPat rp <*> pure (fromHseRPatOp rop)
      E.RPEither l rp1 rp2 -> HaRP.RPEither l <$> fromHseRPat rp1 <*> fromHseRPat rp2
      E.RPSeq l rps        -> HaRP.RPSeq l <$> mapM fromHseRPat rps
      E.RPGuard l p ss     -> HaRP.RPGuard l <$> fromHsePat p <*> mapM fromHseStmt ss
      E.RPCAs l n rp       -> HaRP.RPCAs l (fromHseName n) <$> fromHseRPat rp
      E.RPAs l n rp        -> HaRP.RPAs l (fromHseName n) <$> fromHseRPat rp
      E.RPParen l rp       -> HaRP.RPParen l <$> fromHseRPat rp
      E.RPPat l p          -> HaRP.RPPat l <$> fromHsePat p

fromHsePatField :: HsePat pat => Parsed_ E.PatField -> ParseResult (Parsed (Sugar.PatField pat))
fromHsePatField (E.PFieldPat l qn p) = Sugar.PFieldPat l (fromHseQName qn) <$> fromHsePat p
fromHsePatField (E.PFieldPun l n)    = pure $ Sugar.PFieldPun l (fromHseName n)
fromHsePatField (E.PFieldWildcard l) = pure $ Sugar.PFieldWildcard l

class Typeable t => HseStmt t where
    fromHseStmt :: Parsed_ E.Stmt -> ParseResult (Parsed t)

instance HseStmt NoExts where
    fromHseStmt = fromHseFailed

instance (HseBinds binds, HseExp exp, HsePat pat, HseStmt stmtext)
  => HseStmt (Sugar.Stmt binds exp pat stmtext) where
    fromHseStmt (E.Generator l p e) = Sugar.Generator l <$> fromHsePat p <*> fromHseExp e
    fromHseStmt (E.Qualifier l e)   = Sugar.Qualifier l <$> fromHseExp e
    fromHseStmt (E.LetStmt l bs)    = Sugar.LetStmt l <$> fromHseBinds bs
    fromHseStmt sext                = Sugar.StmtExt <$> fromHseStmt sext

instance HseStmt stmt => HseStmt (Arrows.Stmt stmt) where
    fromHseStmt (E.RecStmt l ss)  = Arrows.RecStmt l <$> mapM fromHseStmt ss
    fromHseStmt s                 = fromHseFailed s


class Typeable t => HseQualStmt t where
    fromHseQualStmt :: Parsed_ E.QualStmt -> ParseResult (Parsed t)

instance (HseBinds binds, HseExp exp, HsePat pat, HseStmt stmtext)
  => HseQualStmt (Sugar.Stmt binds exp pat stmtext) where
    fromHseQualStmt (E.QualStmt _ s) = fromHseStmt s
    fromHseQualStmt qs               = fromHseFailed qs


instance (HseStmt stmt, HseExp exp)
  => HseQualStmt (TransformListComp.QualStmt stmt exp) where
    fromHseQualStmt (E.QualStmt     l s)     = TransformListComp.QualStmt     l <$> fromHseStmt s
    fromHseQualStmt (E.ThenTrans    l e)     = TransformListComp.ThenTrans    l <$> fromHseExp e
    fromHseQualStmt (E.ThenBy       l e1 e2) = TransformListComp.ThenBy       l <$> fromHseExp e1 <*> fromHseExp e2
    fromHseQualStmt (E.GroupBy      l e)     = TransformListComp.GroupBy      l <$> fromHseExp e
    fromHseQualStmt (E.GroupUsing   l e)     = TransformListComp.GroupUsing   l <$> fromHseExp e
    fromHseQualStmt (E.GroupByUsing l e1 e2) = TransformListComp.GroupByUsing l <$> fromHseExp e1 <*> fromHseExp e2

fromHseFieldUpdate :: HseExp exp => Parsed_ E.FieldUpdate -> ParseResult (Parsed (Sugar.FieldUpdate exp))
fromHseFieldUpdate (E.FieldUpdate l qn e) = Sugar.FieldUpdate l (fromHseQName qn) <$> fromHseExp e
fromHseFieldUpdate (E.FieldPun l n)       = pure $ Sugar.FieldPun l (fromHseName n)
fromHseFieldUpdate (E.FieldWildcard l)    = pure $ Sugar.FieldWildcard l

fromHseAlt :: (HseBinds binds, HseGuard guard, HseExp exp, HsePat pat)
           => Parsed_ E.Alt -> ParseResult (Parsed (Sugar.Alt binds guard exp pat))
fromHseAlt (E.Alt l p gs bs) = Sugar.Alt l <$> fromHsePat p <*> fromHseGuardedAlts gs <*> traverse fromHseBinds bs

fromHseGuardedAlts :: (HseGuard guard, HseExp exp) => Parsed_ E.GuardedAlts -> ParseResult (Parsed (Sugar.GuardedAlts guard exp))
fromHseGuardedAlts (E.UnGuardedAlt l e)     = Sugar.UnGuardedAlt l <$> fromHseExp e
fromHseGuardedAlts (E.GuardedAlts  l galts) = Sugar.GuardedAlts  l <$> mapM fromHseGuardedAlt galts

fromHseGuardedAlt :: (HseGuard guard, HseExp exp) => Parsed_ E.GuardedAlt -> ParseResult (Parsed (Sugar.GuardedAlt guard exp))
fromHseGuardedAlt (E.GuardedAlt l ss e) = Sugar.GuardedAlt l <$> fromHseGuard l ss <*> fromHseExp e

class Typeable t => HseGuard t where
    fromHseGuard :: SrcLoc -> [Parsed_ E.Stmt] -> ParseResult (Parsed t)

instance HseStmt stmt => HseGuard (PatternGuards.PatternGuard stmt) where
    fromHseGuard l ss = PatternGuards.PatternGuard l <$> mapM fromHseStmt ss
