module Language.Haskell.Parser.Internal
  -- (
  --  parseModule
  -- ,parseExp
  -- ,module Language.Haskell.Exts.SrcLoc
  -- )

where


import qualified Language.Haskell.Exts.Annotated.Syntax as E
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Exts.SrcLoc

import qualified Language.Haskell.AST.Exts.Arrows as Arrows
import qualified Language.Haskell.AST.Exts.FFI as FFI
import qualified Language.Haskell.AST.Exts.HSX as HSX
import qualified Language.Haskell.AST.Exts.HaRP as HaRP
import qualified Language.Haskell.AST.Exts.ImplicitParams as ImplicitParams
import qualified Language.Haskell.AST.Exts.MDo as MDo
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

--parseModule :: SrcInfo loc => String -> P.ParseResult (Module String loc)
--parseModule = fmap fromHseModule . P.parse

--parseExp :: SrcInfo loc => String -> P.ParseResult (Exp String loc)
--parseExp = fmap fromHseExp . P.parse




--fromHseModuleName :: E.ModuleName l -> ModuleName String l
--fromHseModuleName (E.ModuleName l s) = ModuleName l s

--fromHseSpecialCon :: E.SpecialCon l -> SpecialCon l
--fromHseSpecialCon sc = case sc of
--        E.UnitCon l       -> UnitCon l
--        E.ListCon l       -> ListCon l
--        E.FunCon  l       -> FunCon  l
--        E.TupleCon l b n  -> TupleCon l (fromHseBoxed b) n
--        E.Cons l          -> Cons l
--        E.UnboxedSingleCon l  -> UnboxedSingleCon l

--fromHseQName :: E.QName l -> QName String l
--fromHseQName qn = case qn of
--        E.Qual    l mn n  -> Qual    l (fromHseModuleName mn) (fromHseName n)
--        E.UnQual  l    n  -> UnQual  l (fromHseName n)
--        E.Special l sc    -> Special l (fromHseSpecialCon sc)

--fromHseName :: E.Name l -> Name String l
--fromHseName (E.Ident  l s) = Ident  l s
--fromHseName (E.Symbol l s) = Symbol l s

--fromHseIPName :: E.IPName l -> IPName String l
--fromHseIPName (E.IPDup l s) = IPDup l s
--fromHseIPName (E.IPLin l s) = IPLin l s

--fromHseQOp :: E.QOp l -> QOp String l
--fromHseQOp (E.QVarOp l qn) = QVarOp l (fromHseQName qn)
--fromHseQOp (E.QConOp l qn) = QConOp l (fromHseQName qn)

--fromHseOp :: E.Op l -> Op String l
--fromHseOp (E.VarOp l n) = VarOp l (fromHseName n)
--fromHseOp (E.ConOp l n) = ConOp l (fromHseName n)

--fromHseCName :: E.CName l -> CName String l
--fromHseCName (E.VarName l n) = VarName l (fromHseName n)
--fromHseCName (E.ConName l n) = ConName l (fromHseName n)

--fromHseModule :: E.Module l -> Module String l
--fromHseModule (E.Module l mmh ops iss dcls) =
--        Module l (fmap fromHseModuleHead mmh) (map fromHseModulePragma ops) (map fromHseImportDecl iss) (map fromHseDecl dcls)
--fromHseModule (E.XmlPage l mn os xn xas me es) =
--        XmlPage l (fromHseModuleName mn) (map fromHseModulePragma os) (fromHseXName xn) (map fromHseXAttr xas) (fmap fromHseExp me) (map fromHseExp es)
--fromHseModule (E.XmlHybrid l mmh ops iss dcls xn xas me es) =
--        XmlHybrid l (fmap fromHseModuleHead mmh) (map fromHseModulePragma ops) (map fromHseImportDecl iss) (map fromHseDecl dcls)
--                (fromHseXName xn) (map fromHseXAttr xas) (fmap fromHseExp me) (map fromHseExp es)

--fromHseModuleHead :: E.ModuleHead l -> ModuleHead String l
--fromHseModuleHead (E.ModuleHead l mn mwt mexpl) =
--        ModuleHead l (fromHseModuleName mn) (fmap fromHseWarningText mwt) (fmap fromHseExportSpecList mexpl)

--fromHseExportSpecList :: E.ExportSpecList l -> ExportSpecList String l
--fromHseExportSpecList (E.ExportSpecList l ess) = ExportSpecList l (map fromHseExportSpec ess)

--fromHseExportSpec :: E.ExportSpec l -> ExportSpec String l
--fromHseExportSpec es = case es of
--        E.EVar l qn       -> EVar l (fromHseQName qn)
--        E.EAbs l qn       -> EAbs l (fromHseQName qn)
--        E.EThingAll l qn  -> EThingAll l (fromHseQName qn)
--        E.EThingWith l qn cns -> EThingWith l (fromHseQName qn) (map fromHseCName cns)
--        E.EModuleContents l mn    -> EModuleContents l (fromHseModuleName mn)
--        -- E.EType l m       -> EType l (fromHseXxx m)

--fromHseImportDecl :: E.ImportDecl l -> ImportDecl String l
--fromHseImportDecl (E.ImportDecl l mn qual src pkg mmn mis) =
--        ImportDecl l (fromHseModuleName mn) qual src pkg (fmap fromHseModuleName mmn) (fmap fromHseImportSpecList mis)

--fromHseImportSpecList :: E.ImportSpecList l -> ImportSpecList String l
--fromHseImportSpecList (E.ImportSpecList l b iss) = ImportSpecList l b (map fromHseImportSpec iss)

--fromHseImportSpec :: E.ImportSpec l -> ImportSpec String l
--fromHseImportSpec is = case is of
--        E.IVar l n        -> IVar l (fromHseName n)
--        E.IAbs l n        -> IAbs l (fromHseName n)
--        E.IThingAll l n   -> IThingAll l (fromHseName n)
--        E.IThingWith l n cns  -> IThingWith l (fromHseName n) (map fromHseCName cns)
--        -- E.IType l m       -> IType l (fromHseXxx m)

--fromHseAssoc :: E.Assoc l -> Assoc l
--fromHseAssoc (E.AssocNone  l) = AssocNone  l
--fromHseAssoc (E.AssocLeft  l) = AssocLeft  l
--fromHseAssoc (E.AssocRight l) = AssocRight l

--fromHseDecl :: E.Decl l -> Decl String l
--fromHseDecl decl = case decl of
--        E.TypeDecl     l dh t      -> TypeDecl    l (fromHseDeclHead dh) (fromHseType t)
--        E.TypeFamDecl  l dh mk     -> TypeFamDecl l (fromHseDeclHead dh) (fmap fromHseKind mk)
--        E.DataDecl     l dn mcx dh cds ders ->
--            DataDecl l (fromHseDataOrNew dn) (fmap fromHseContext mcx) (fromHseDeclHead dh) (map fromHseQualConDecl cds) (fmap fromHseDeriving ders)
--        E.GDataDecl    l dn mcx dh mk gds ders ->
--            GDataDecl l (fromHseDataOrNew dn) (fmap fromHseContext mcx) (fromHseDeclHead dh) (fmap fromHseKind mk) (map fromHseGadtDecl gds) (fmap fromHseDeriving ders)
--        E.DataFamDecl  l mcx dh mk          -> DataFamDecl l (fmap fromHseContext mcx) (fromHseDeclHead dh) (fmap fromHseKind mk)
--        E.TypeInsDecl  l t1 t2              -> TypeInsDecl l (fromHseType t1) (fromHseType t2)
--        E.DataInsDecl  l dn t cds ders      -> DataInsDecl l (fromHseDataOrNew dn) (fromHseType t) (map fromHseQualConDecl cds) (fmap fromHseDeriving ders)
--        E.GDataInsDecl l dn t mk gds ders   -> GDataInsDecl l (fromHseDataOrNew dn) (fromHseType t) (fmap fromHseKind mk) (map fromHseGadtDecl gds) (fmap fromHseDeriving ders)
--        E.ClassDecl    l mcx dh fds mcds    -> ClassDecl l (fmap fromHseContext mcx) (fromHseDeclHead dh) (map fromHseFunDep fds) (fmap (map fromHseClassDecl) mcds)
--        E.InstDecl     l mcx ih mids        -> InstDecl  l (fmap fromHseContext mcx) (fromHseInstHead ih) (fmap (map fromHseInstDecl) mids)
--        E.DerivDecl    l mcx ih             -> DerivDecl l (fmap fromHseContext mcx) (fromHseInstHead ih)
--        E.InfixDecl    l a k ops            -> InfixDecl l (fromHseAssoc a) k (map fromHseOp ops)
--        E.DefaultDecl  l ts                 -> DefaultDecl l (map fromHseType ts)
--        E.SpliceDecl   l sp                 -> SpliceDecl l (fromHseExp sp)
--        E.TypeSig      l ns t               -> TypeSig l (map fromHseName ns) (fromHseType t)
--        E.FunBind      l ms                 -> FunBind l (map fromHseMatch ms)
--        E.PatBind      l p mt rhs bs        -> PatBind l (fromHsePat p) (fmap fromHseType mt) (fromHseRhs rhs) (fmap fromHseBinds bs)
--        E.ForImp       l cc msf s n t       -> ForImp l (fromHseCallConv cc) (fmap fromHseSafety msf) s (fromHseName n) (fromHseType t)
--        E.ForExp       l cc     s n t       -> ForExp l (fromHseCallConv cc)                     s (fromHseName n) (fromHseType t)
--        E.RulePragmaDecl   l rs             -> RulePragmaDecl l (map fromHseRule rs)
--        E.DeprPragmaDecl   l nss            -> DeprPragmaDecl l (map wp nss)
--        E.WarnPragmaDecl   l nss            -> WarnPragmaDecl l (map wp nss)
--        E.InlineSig        l b mact qn      -> InlineSig l b (fmap fromHseActivation mact) (fromHseQName qn)
--        E.InlineConlikeSig l   mact qn      -> InlineConlikeSig l (fmap fromHseActivation mact) (fromHseQName qn)
--        E.SpecInlineSig    l b mact qn ts   -> SpecInlineSig l b (fmap fromHseActivation mact) (fromHseQName qn) (map fromHseType ts)
--        E.SpecSig          l   mact qn ts   -> SpecSig       l   (fmap fromHseActivation mact) (fromHseQName qn) (map fromHseType ts)
--        E.InstSig          l mcx ih         -> InstSig l (fmap fromHseContext mcx) (fromHseInstHead ih)
--        E.AnnPragma        l ann            -> AnnPragma l (fromHseAnnotation ann)
--      where wp (ns, s) = (map fromHseName ns, s)

--fromHseAnnotation :: E.Annotation l -> Annotation String l
--fromHseAnnotation (E.Ann     l n e) = Ann     l (fromHseName n) (fromHseExp e)
--fromHseAnnotation (E.TypeAnn l n e) = TypeAnn l (fromHseName n) (fromHseExp e)
--fromHseAnnotation (E.ModuleAnn l e) = ModuleAnn l (fromHseExp e)

--fromHseDataOrNew :: E.DataOrNew l -> DataOrNew l
--fromHseDataOrNew (E.DataType l) = DataType l
--fromHseDataOrNew (E.NewType  l) = NewType  l

--fromHseDeclHead :: E.DeclHead l -> DeclHead String l
--fromHseDeclHead (E.DHead l n tvs)       = DHead l (fromHseName n) (map fromHseTyVarBind tvs)
--fromHseDeclHead (E.DHInfix l tva n tvb) = DHInfix l (fromHseTyVarBind tva) (fromHseName n) (fromHseTyVarBind tvb)
--fromHseDeclHead (E.DHParen l dh)        = DHParen l (fromHseDeclHead dh)

--fromHseInstHead :: E.InstHead l -> InstHead String l
--fromHseInstHead (E.IHead l qn ts)       = IHead l (fromHseQName qn) (map fromHseType ts)
--fromHseInstHead (E.IHInfix l ta qn tb)  = IHInfix l (fromHseType ta) (fromHseQName qn) (fromHseType tb)
--fromHseInstHead (E.IHParen l ih)        = IHParen l (fromHseInstHead ih)

--fromHseDeriving :: E.Deriving l -> Deriving String l
--fromHseDeriving (E.Deriving l ihs) = Deriving l (map fromHseInstHead ihs)

--fromHseBinds :: E.Binds l -> Binds String l
--fromHseBinds (E.BDecls  l decls) = BDecls l (map fromHseDecl decls)
--fromHseBinds (E.IPBinds l ibs)   = IPBinds l (map fromHseIPBind ibs)

--fromHseIPBind :: E.IPBind l -> IPBind String l
--fromHseIPBind (E.IPBind l ipn e) = IPBind l (fromHseIPName ipn) (fromHseExp e)

--fromHseMatch :: E.Match l -> Match String l
--fromHseMatch (E.Match l n ps rhs bs) =
--        Match l (fromHseName n) (map fromHsePat ps) (fromHseRhs rhs) (fmap fromHseBinds bs)
--fromHseMatch (E.InfixMatch l a n ps rhs bs) =
--        InfixMatch l (fromHsePat a) (fromHseName n) (map fromHsePat ps) (fromHseRhs rhs) (fmap fromHseBinds bs)

--fromHseQualConDecl :: E.QualConDecl l -> QualConDecl String l
--fromHseQualConDecl (E.QualConDecl l mtvs mcx cd) = QualConDecl l (fmap (map fromHseTyVarBind) mtvs) (fmap fromHseContext mcx) (fromHseConDecl cd)

--fromHseConDecl :: E.ConDecl l -> ConDecl String l
--fromHseConDecl (E.ConDecl l n bts) = ConDecl l (fromHseName n) (map fromHseBangType bts)
--fromHseConDecl (E.InfixConDecl l ta n tb) = InfixConDecl l (fromHseBangType ta) (fromHseName n) (fromHseBangType tb)
--fromHseConDecl (E.RecDecl l n fds) = RecDecl l (fromHseName n) (map fromHseFieldDecl fds)

--fromHseFieldDecl :: E.FieldDecl l -> FieldDecl String l
--fromHseFieldDecl (E.FieldDecl l ns t) = FieldDecl l (map fromHseName ns) (fromHseBangType t)

--fromHseGadtDecl :: E.GadtDecl l -> GadtDecl String l
--fromHseGadtDecl (E.GadtDecl l n t) = GadtDecl l (fromHseName n) (fromHseType t)

--fromHseClassDecl :: E.ClassDecl l -> ClassDecl String l
--fromHseClassDecl (E.ClsDecl    l d) = ClsDecl l (fromHseDecl d)
--fromHseClassDecl (E.ClsDataFam l mcx dh mk) = ClsDataFam l (fmap fromHseContext mcx) (fromHseDeclHead dh) (fmap fromHseKind mk)
--fromHseClassDecl (E.ClsTyFam   l     dh mk) = ClsTyFam   l                     (fromHseDeclHead dh) (fmap fromHseKind mk)
--fromHseClassDecl (E.ClsTyDef   l t1 t2) = ClsTyDef l (fromHseType t1) (fromHseType t2)

--fromHseInstDecl :: E.InstDecl l -> InstDecl String l
--fromHseInstDecl id = case id of
--        E.InsDecl   l d           -> InsDecl l (fromHseDecl d)
--        E.InsType   l t1 t2       -> InsType l (fromHseType t1) (fromHseType t2)
--        E.InsData   l dn t    cds ders
--            -> InsData  l (fromHseDataOrNew dn) (fromHseType t)  (map fromHseQualConDecl cds) (fmap fromHseDeriving ders)
--        E.InsGData  l dn t mk gds ders
--            -> InsGData l (fromHseDataOrNew dn) (fromHseType t) (fmap fromHseKind mk) (map fromHseGadtDecl gds) (fmap fromHseDeriving ders)

--fromHseBangType :: E.BangType l -> BangType String l
--fromHseBangType (E.BangedTy   l t) = BangedTy l (fromHseType t)
--fromHseBangType (E.UnBangedTy l t) = UnBangedTy l (fromHseType t)
--fromHseBangType (E.UnpackedTy l t) = UnpackedTy l (fromHseType t)

--fromHseRhs :: E.Rhs l -> Rhs String l
--fromHseRhs (E.UnGuardedRhs l e) = UnGuardedRhs l (fromHseExp e)
--fromHseRhs (E.GuardedRhss  l grhss) = GuardedRhss l (map fromHseGuardedRhs grhss)

--fromHseGuardedRhs :: E.GuardedRhs l -> GuardedRhs String l
--fromHseGuardedRhs (E.GuardedRhs l ss e) = GuardedRhs l (map fromHseStmt ss) (fromHseExp e)

--fromHseType :: E.Type l -> Type String l
--fromHseType t = case t of
--      E.TyForall l mtvs mcx t         -> TyForall l (fmap (map fromHseTyVarBind) mtvs) (fmap fromHseContext mcx) (fromHseType t)
--      E.TyFun   l t1 t2               -> TyFun l (fromHseType t1) (fromHseType t2)
--      E.TyTuple l b ts                -> TyTuple l (fromHseBoxed b) (map fromHseType ts)
--      E.TyList  l t                   -> TyList l (fromHseType t)
--      E.TyApp   l t1 t2               -> TyApp l (fromHseType t1) (fromHseType t2)
--      E.TyVar   l n                   -> TyVar l (fromHseName n)
--      E.TyCon   l qn                  -> TyCon l (fromHseQName qn)
--      E.TyParen l t                   -> TyParen l (fromHseType t)
--      E.TyInfix l ta qn tb            -> TyInfix l (fromHseType ta) (fromHseQName qn) (fromHseType tb)
--      E.TyKind  l t k                 -> TyKind l (fromHseType t) (fromHseKind k)
--      -- E.TyPromoted l   p              -> TyPromoted l   (fromHsePat p)

--fromHseBoxed :: E.Boxed -> Boxed
--fromHseBoxed E.Boxed = Boxed
--fromHseBoxed E.Unboxed = Unboxed

--fromHseTyVarBind :: E.TyVarBind l -> TyVarBind String l
--fromHseTyVarBind (E.KindedVar   l n k) = KindedVar l (fromHseName n) (fromHseKind k)
--fromHseTyVarBind (E.UnkindedVar l n)   = UnkindedVar l (fromHseName n)

--fromHseKind :: E.Kind l -> Kind String l
--fromHseKind (E.KindStar  l)   = KindStar l
--fromHseKind (E.KindBang  l)   = KindBang l
--fromHseKind (E.KindFn    l k1 k2) = KindFn l (fromHseKind k1) (fromHseKind k2)
--fromHseKind (E.KindParen l k) = KindParen l (fromHseKind k)
--fromHseKind (E.KindVar   l n) = KindVar l (fromHseName n)
---- fromHseKind (E.KindApp   l k1 k2) = KindFn l (fromHseKind k1) (fromHseKind k2)
---- fromHseKind (E.KindTuple l ks) = KindTuple l (map fromHseKind ks)
---- fromHseKind (E.KindList  l ks) = KindList  l (map fromHseKind ks)

--fromHseFunDep :: E.FunDep l -> FunDep String l
--fromHseFunDep (E.FunDep l ns1 ns2) = FunDep l (map fromHseName ns1) (map fromHseName ns2)

--fromHseContext :: E.Context l -> Context String l
--fromHseContext (E.CxSingle l asst) = CxSingle l (fromHseAsst asst)
--fromHseContext (E.CxTuple l assts) = CxTuple l (map fromHseAsst assts)
--fromHseContext (E.CxParen l ctxt)  = CxParen l (fromHseContext ctxt)
--fromHseContext (E.CxEmpty l)       = CxEmpty l

--fromHseAsst :: E.Asst l -> Asst String l
--fromHseAsst asst = case asst of
--        E.ClassA l qn ts      -> ClassA l (fromHseQName qn) (map fromHseType ts)
--        E.InfixA l ta qn tb   -> InfixA l (fromHseType ta) (fromHseQName qn) (fromHseType tb)
--        E.IParam l ipn t      -> IParam l (fromHseIPName ipn) (fromHseType t)
--        E.EqualP l t1 t2      -> EqualP l (fromHseType t1) (fromHseType t2)

--fromHseLiteral :: E.Literal l -> Literal
--fromHseLiteral lit = case lit of
--        E.Char    l c rw    -> Char   (ExactRep rw) c
--        E.String  l s rw    -> String (ExactRep rw) s
--        E.Int     l i rw    -> Int    (ExactRep rw) i
--        E.Frac    l r rw    -> Frac   (ExactRep rw) r
--        E.PrimInt    l i rw -> PrimInt    (ExactRep rw) i
--        E.PrimWord   l i rw -> PrimWord   (ExactRep rw) i
--        E.PrimFloat  l r rw -> PrimFloat  (ExactRep rw) r
--        E.PrimDouble l r rw -> PrimDouble (ExactRep rw) r
--        E.PrimChar   l c rw -> PrimChar   (ExactRep rw) c
--        E.PrimString l s rw -> PrimString (ExactRep rw) s

--fromHseExp :: E.Exp l -> Exp String l
--fromHseExp e = case e of
--        E.Var l qn        -> Var l (fromHseQName qn)
--        E.IPVar l ipn     -> IPVar l (fromHseIPName ipn)
--        E.Con l qn        -> Con l (fromHseQName qn)
--        E.Lit l lit       -> Lit l (fromHseLiteral lit)
--        E.InfixApp l e1 qop e2    -> InfixApp l (fromHseExp e1) (fromHseQOp qop) (fromHseExp e2)
--        E.App l e1 e2     -> App l (fromHseExp e1) (fromHseExp e2)
--        E.NegApp l e      -> NegApp l (fromHseExp e)
--        E.Lambda l ps e   -> Lambda l (map fromHsePat ps) (fromHseExp e)
--        E.Let l bs e      -> Let l (fromHseBinds bs) (fromHseExp e)
--        E.If l ec et ee   -> If l (fromHseExp ec) (fromHseExp et) (fromHseExp ee)
--        E.Case l e alts   -> Case l (fromHseExp e) (map fromHseAlt alts)
--        E.Do l ss         -> Do l (map fromHseStmt ss)
--        E.MDo l ss        -> MDo l (map fromHseStmt ss)
--        E.Tuple l bx es   -> Tuple l (fromHseBoxed bx) (map fromHseExp es)
--        E.TupleSection l bx mes -> TupleSection l (fromHseBoxed bx) (map (fmap fromHseExp) mes)
--        E.List l es       -> List l (map fromHseExp es)
--        E.Paren l e       -> Paren l (fromHseExp e)
--        E.LeftSection l e qop     -> LeftSection l (fromHseExp e) (fromHseQOp qop)
--        E.RightSection l qop e    -> RightSection l (fromHseQOp qop) (fromHseExp e)
--        E.RecConstr l qn fups     -> RecConstr l (fromHseQName qn) (map fromHseFieldUpdate fups)
--        E.RecUpdate l e  fups     -> RecUpdate l (fromHseExp e) (map fromHseFieldUpdate fups)
--        E.EnumFrom l e            -> EnumFrom l (fromHseExp e)
--        E.EnumFromTo l ef et      -> EnumFromTo l (fromHseExp ef) (fromHseExp et)
--        E.EnumFromThen l ef et    -> EnumFromThen l (fromHseExp ef) (fromHseExp et)
--        E.EnumFromThenTo l ef eth eto -> EnumFromThenTo l (fromHseExp ef) (fromHseExp eth) (fromHseExp eto)
--        E.ListComp l e qss        -> ListComp l (fromHseExp e) (map fromHseQualStmt qss)
--        E.ParComp  l e qsss       -> ParComp  l (fromHseExp e) (map (map fromHseQualStmt) qsss)
--        E.ExpTypeSig l e t        -> ExpTypeSig l (fromHseExp e) (fromHseType t)
--        E.VarQuote l qn           -> VarQuote l (fromHseQName qn)
--        E.TypQuote l qn           -> TypQuote l (fromHseQName qn)
--        E.BracketExp l br         -> BracketExp l (fromHseBracket br)
--        E.SpliceExp l sp          -> SpliceExp l (fromHseSplice sp)
--        E.QuasiQuote l sn se      -> QuasiQuote l sn se

--        E.XTag  l xn xas me es     -> XTag  l (fromHseXName xn) (map fromHseXAttr xas) (fmap fromHseExp me) (map fromHseExp es)
--        E.XETag l xn xas me        -> XETag l (fromHseXName xn) (map fromHseXAttr xas) (fmap fromHseExp me)
--        E.XPcdata l s              -> XPcdata l s
--        E.XExpTag l e              -> XExpTag l (fromHseExp e)
--        E.XChildTag l es           -> XChildTag l (map fromHseExp es)

--        E.CorePragma l s e   -> CorePragma l s (fromHseExp e)
--        E.SCCPragma  l s e   -> SCCPragma l s (fromHseExp e)
--        E.GenPragma  l s n12 n34 e -> GenPragma l s n12 n34 (fromHseExp e)

--        E.Proc            l p e   -> Proc l (fromHsePat p) (fromHseExp e)
--        E.LeftArrApp      l e1 e2 -> LeftArrApp      l (fromHseExp e1) (fromHseExp e2)
--        E.RightArrApp     l e1 e2 -> RightArrApp     l (fromHseExp e1) (fromHseExp e2)
--        E.LeftArrHighApp  l e1 e2 -> LeftArrHighApp  l (fromHseExp e1) (fromHseExp e2)
--        E.RightArrHighApp l e1 e2 -> RightArrHighApp l (fromHseExp e1) (fromHseExp e2)

--        -- E.LCase l alts -> LCase l (map fromHseAlt alts)

--fromHseXName :: E.XName l -> XName String l
--fromHseXName (E.XName l s)  = XName l s
--fromHseXName (E.XDomName l sd sn) = XDomName l sd sn

--fromHseXAttr :: E.XAttr l -> XAttr String l
--fromHseXAttr (E.XAttr l xn e) = XAttr l (fromHseXName xn) (fromHseExp e)

--fromHseBracket :: E.Bracket l -> Bracket String l
--fromHseBracket (E.ExpBracket l e) = ExpBracket l (fromHseExp e)
--fromHseBracket (E.PatBracket l p) = PatBracket l (fromHsePat p)
--fromHseBracket (E.TypeBracket l t) = TypeBracket l (fromHseType t)
--fromHseBracket (E.DeclBracket l ds) = DeclBracket l (map fromHseDecl ds)

--fromHseSplice :: E.Splice l -> Splice String l
--fromHseSplice (E.IdSplice l s) = IdSplice l s
--fromHseSplice (E.ParenSplice l e) = ParenSplice l (fromHseExp e)

--fromHseSafety :: E.Safety l -> Safety l
--fromHseSafety (E.PlayRisky l) = PlayRisky l
--fromHseSafety (E.PlaySafe l b) = PlaySafe l b
--fromHseSafety (E.PlayInterruptible l) = PlayInterruptible l

--fromHseCallConv :: E.CallConv l -> CallConv l
--fromHseCallConv (E.StdCall l) = StdCall l
--fromHseCallConv (E.CCall l) = CCall l
--fromHseCallConv (E.CPlusPlus l) = CPlusPlus l
--fromHseCallConv (E.DotNet l) = DotNet l
--fromHseCallConv (E.Jvm l) = Jvm l
--fromHseCallConv (E.Js l) = Js l
--fromHseCallConv (E.CApi l) = CApi l

--fromHseModulePragma :: E.ModulePragma l -> ModulePragma String l
--fromHseModulePragma (E.LanguagePragma   l ns) = LanguagePragma l (map fromHseName ns)
--fromHseModulePragma (E.OptionsPragma    l mt s) = OptionsPragma l (fmap fromHseTool mt) s
--fromHseModulePragma (E.AnnModulePragma  l ann) = AnnModulePragma l (fromHseAnnotation ann)

--fromHseTool :: E.Tool -> Tool
--fromHseTool t = case t of
--  E.GHC -> GHC
--  E.HUGS -> HUGS
--  E.NHC98 -> NHC98
--  E.YHC -> YHC
--  E.HADDOCK -> HADDOCK
--  E.UnknownTool s -> UnknownTool s

--fromHseActivation :: E.Activation l -> Activation l
--fromHseActivation (E.ActiveFrom   l k) = ActiveFrom l k
--fromHseActivation (E.ActiveUntil  l k) = ActiveUntil l k

--fromHseRule :: E.Rule l -> Rule String l
--fromHseRule (E.Rule l s mact mrvs e1 e2) =
--        Rule l s (fmap fromHseActivation mact) (fmap (map fromHseRuleVar) mrvs) (fromHseExp e1) (fromHseExp e2)

--fromHseRuleVar :: E.RuleVar l -> RuleVar String l
--fromHseRuleVar (E.RuleVar l n) = RuleVar l (fromHseName n)
--fromHseRuleVar (E.TypedRuleVar l n t) = TypedRuleVar l (fromHseName n) (fromHseType t)

--fromHseWarningText :: E.WarningText l -> WarningText l
--fromHseWarningText (E.DeprText l s) = DeprText l s
--fromHseWarningText (E.WarnText l s) = WarnText l s

--fromHsePat :: E.Pat l -> Pat String l
--fromHsePat p = case p of
--      E.PVar l n          -> PVar l (fromHseName n)
--      E.PLit l lit        -> PLit l (fromHseLiteral lit)
--      E.PNeg l p          -> PNeg l (fromHsePat p)
--      E.PNPlusK l n k     -> PNPlusK l (fromHseName n) k
--      E.PInfixApp l pa qn pb  -> PInfixApp l (fromHsePat pa) (fromHseQName qn) (fromHsePat pb)
--      E.PApp l qn ps      -> PApp l (fromHseQName qn) (map fromHsePat ps)
--      E.PTuple l bx ps    -> PTuple l (fromHseBoxed bx) (map fromHsePat ps)
--      E.PList l ps        -> PList l (map fromHsePat ps)
--      E.PParen l p        -> PParen l (fromHsePat p)
--      E.PRec l qn pfs     -> PRec l (fromHseQName qn) (map fromHsePatField pfs)
--      E.PAsPat l n p      -> PAsPat l (fromHseName n) (fromHsePat p)
--      E.PWildCard l       -> PWildCard l
--      E.PIrrPat l p       -> PIrrPat l (fromHsePat p)
--      E.PatTypeSig l p t  -> PatTypeSig l (fromHsePat p) (fromHseType t)
--      E.PViewPat l e p    -> PViewPat l (fromHseExp e) (fromHsePat p)
--      E.PRPat l rps       -> PRPat l (map fromHseRPat rps)
--      E.PXTag l xn pxas mp ps -> PXTag l (fromHseXName xn) (map fromHsePXAttr pxas) (fmap fromHsePat mp) (map fromHsePat ps)
--      E.PXETag l xn pxas mp   -> PXETag l (fromHseXName xn) (map fromHsePXAttr pxas) (fmap fromHsePat mp)
--      E.PXPcdata l s      -> PXPcdata l s
--      E.PXPatTag l p      -> PXPatTag l (fromHsePat p)
--      E.PXRPats  l rps    -> PXRPats  l (map fromHseRPat rps)
--      E.PExplTypeArg l qn t   -> PExplTypeArg l (fromHseQName qn) (fromHseType t)
--      E.PQuasiQuote l sn st   -> PQuasiQuote l sn st
--      E.PBangPat l p          -> PBangPat l (fromHsePat p)

--fromHsePXAttr :: E.PXAttr l -> PXAttr String l
--fromHsePXAttr (E.PXAttr l xn p) = PXAttr l (fromHseXName xn) (fromHsePat p)

--fromHseRPatOp :: E.RPatOp l -> RPatOp l
--fromHseRPatOp (E.RPStar  l) = RPStar l
--fromHseRPatOp (E.RPStarG l) = RPStarG l
--fromHseRPatOp (E.RPPlus  l) = RPPlus l
--fromHseRPatOp (E.RPPlusG l) = RPPlusG l
--fromHseRPatOp (E.RPOpt   l) = RPOpt l
--fromHseRPatOp (E.RPOptG  l) = RPOptG l

--fromHseRPat :: E.RPat l -> RPat String l
--fromHseRPat rp = case rp of
--      E.RPOp l rp rop         -> RPOp l (fromHseRPat rp) (fromHseRPatOp rop)
--      E.RPEither l rp1 rp2    -> RPEither l (fromHseRPat rp1) (fromHseRPat rp2)
--      E.RPSeq l rps           -> RPSeq l (map fromHseRPat rps)
--      E.RPGuard l p ss        -> RPGuard l (fromHsePat p) (map fromHseStmt ss)
--      E.RPCAs l n rp          -> RPCAs l (fromHseName n) (fromHseRPat rp)
--      E.RPAs l n rp           -> RPAs l (fromHseName n) (fromHseRPat rp)
--      E.RPParen l rp          -> RPParen l (fromHseRPat rp)
--      E.RPPat l p             -> RPPat l (fromHsePat p)

--fromHsePatField :: E.PatField l -> PatField String l
--fromHsePatField (E.PFieldPat l qn p) = PFieldPat l (fromHseQName qn) (fromHsePat p)
--fromHsePatField (E.PFieldPun l n) = PFieldPun l (fromHseName n)
--fromHsePatField (E.PFieldWildcard l) = PFieldWildcard l

--fromHseStmt :: E.Stmt l -> Stmt String l
--fromHseStmt (E.Generator l p e) = Generator l (fromHsePat p) (fromHseExp e)
--fromHseStmt (E.Qualifier l e)   = Qualifier l (fromHseExp e)
--fromHseStmt (E.LetStmt l bs)    = LetStmt l (fromHseBinds bs)
--fromHseStmt (E.RecStmt l ss)    = RecStmt l (map fromHseStmt ss)

--fromHseQualStmt :: E.QualStmt l -> QualStmt String l
--fromHseQualStmt (E.QualStmt     l s) = QualStmt l (fromHseStmt s)
--fromHseQualStmt (E.ThenTrans    l e) = ThenTrans l (fromHseExp e)
--fromHseQualStmt (E.ThenBy       l e1 e2) = ThenBy l (fromHseExp e1) (fromHseExp e2)
--fromHseQualStmt (E.GroupBy      l e) = GroupBy l (fromHseExp e)
--fromHseQualStmt (E.GroupUsing   l e) = GroupUsing l (fromHseExp e)
--fromHseQualStmt (E.GroupByUsing l e1 e2) = GroupByUsing l (fromHseExp e1) (fromHseExp e2)

--fromHseFieldUpdate :: E.FieldUpdate l -> FieldUpdate String l
--fromHseFieldUpdate (E.FieldUpdate l qn e) = FieldUpdate l (fromHseQName qn) (fromHseExp e)
--fromHseFieldUpdate (E.FieldPun l n)       = FieldPun l (fromHseName n)
--fromHseFieldUpdate (E.FieldWildcard l)    = FieldWildcard l

--fromHseAlt :: E.Alt l -> Alt String l
--fromHseAlt (E.Alt l p gs bs) = Alt l (fromHsePat p) (fromHseGuardedAlts gs) (fmap fromHseBinds bs)

--fromHseGuardedAlts :: E.GuardedAlts l -> GuardedAlts String l
--fromHseGuardedAlts (E.UnGuardedAlt l e) = UnGuardedAlt l (fromHseExp e)
--fromHseGuardedAlts (E.GuardedAlts  l galts) = GuardedAlts l (map fromHseGuardedAlt galts)

--fromHseGuardedAlt :: E.GuardedAlt l -> GuardedAlt String l
--fromHseGuardedAlt (E.GuardedAlt l ss e) = GuardedAlt l (map fromHseStmt ss) (fromHseExp e)
