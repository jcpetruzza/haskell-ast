{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Core

where


import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- | The name of a Haskell module.
data ModuleName id l = ModuleName l id
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.
data SpecialCon l
    = UnitCon l             -- ^ unit type and data constructor @()@
    | ListCon l             -- ^ list type constructor @[]@
    | FunCon  l             -- ^ function type constructor @->@
    | TupleCon l Boxed Int  -- ^ /n/-ary tuple type and data
                            --   constructors @(,)@ etc, possibly boxed @(\#,\#)@
    | Cons l                -- ^ list data constructor @(:)@
    | UnboxedSingleCon l    -- ^ unboxed singleton tuple constructor @(\# \#)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data QName id l
    = Qual    l (ModuleName id l) (Name id l)    -- ^ name qualified with a module name
    | UnQual  l                   (Name id l)    -- ^ unqualified local name
    | Special l (SpecialCon l)  -- ^ built-in constructor with special syntax
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | This type is used to represent variables, and also constructors.
data Name id l
    = Ident  l id   -- ^ /varid/ or /conid/.
    | Symbol l id   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Operators appearing in @infix@ declarations are never qualified.
data Op id l
    = VarOp l (Name id l)    -- ^ variable operator (/varop/)
    | ConOp l (Name id l)    -- ^ constructor operator (/conop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data CName id l
    = VarName l (Name id l) -- ^ name of a method or field
    | ConName l (Name id l) -- ^ name of a data constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A complete Haskell source module.
data Module bind tydecl classreldecl declext mpragext id l
    = Module l (Maybe (ModuleHead id l)) [ModulePragma mpragext id l] [ImportDecl id l] [Decl bind tydecl classreldecl declext id l]
    -- ^ an ordinary Haskell module
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The head of a module, including the name and export specification.
data ModuleHead id l = ModuleHead l (ModuleName id l) (Maybe (WarningText l)) (Maybe (ExportSpecList id l))
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Warning text to optionally use in the module header of e.g.
--   a deprecated module.
data WarningText l
    = DeprText l String
    | WarnText l String
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | An explicit export specification. The 'Bool' is 'True' if the export has
-- the @type@ keyword (@-XExplicitNamespaces@)
data ExportSpecList id l
    = ExportSpecList l [ExportSpec id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An item in a module's export specification.
data ExportSpec id l
     = EVar l (QName id l)                 -- ^ variable
     | EAbs l (QName id l)                 -- ^ @T@:
                                           --   a class or datatype exported abstractly,
                                           --   or a type synonym.
     | EThingAll l (QName id l)            -- ^ @T(..)@:
                                           --   a class exported with all of its methods, or
                                           --   a datatype exported with all of its constructors.
     | EThingWith l (QName id l) [CName id l]     -- ^ @T(C_1,...,C_n)@:
                                           --   a class exported with some of its methods, or
                                           --   a datatype exported with some of its constructors.
     | EModuleContents l (ModuleName id l) -- ^ @module M@:
                                           --   re-export a module.
     -- | EType id l (ExportSpec id l)           -- ^ @type x@: available with @-XExplicitNamespaces@

  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An import declaration.
data ImportDecl id l = ImportDecl
    { importAnn :: l                             -- ^ annotation, used by parser for position of the @import@ keyword.
    , importModule :: ModuleName id l            -- ^ name of the module imported.
    , importQualified :: Bool                    -- ^ imported @qualified@?
    , importSrc :: Bool                          -- ^ imported with @{-\# SOURCE \#-}@?
    , importPkg :: Maybe String                  -- ^ imported with explicit package name
    , importAs :: Maybe (ModuleName id l)        -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe (ImportSpecList id l)
            -- ^ optional list of import specifications.
    }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An explicit import specification list.
data ImportSpecList id l
    = ImportSpecList l Bool [ImportSpec id l]
            -- A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
            --
            -- The other 'Bool' is true if the 'ImportSpec' has
            -- the @type@ keyword @-XExplicitNamespaces@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec id l
     = IVar l (Name id l)           -- ^ variable
     | IAbs l (Name id l)           -- ^ @T@:
                                    --   the name of a class, datatype or type synonym.
     | IThingAll l (Name id l)      -- ^ @T(..)@:
                                    --   a class imported with all of its methods, or
                                    --   a datatype imported with all of its constructors.
     | IThingWith l (Name id l) [CName id l]    -- ^ @T(C_1,...,C_n)@:
                                    --   a class imported with some of its methods, or
                                    --   a datatype imported with some of its constructors.
     -- | IType l (ImportSpec id l)    -- ^ @type ...@ (-XExplicitNamespaces)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Associativity of an operator.
data Assoc l
     = AssocNone  l -- ^ non-associative operator (declared with @infix@)
     | AssocLeft  l -- ^ left-associative operator (declared with @infixl@).
     | AssocRight l -- ^ right-associative operator (declared with @infixr@)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A top-level declaration.
data Decl bind tydecl classreldecl declext id l
     = TyDecl       l (tydecl id l)
     -- ^ A declaration of a type
     | BindDecl     l (bind id l)
     -- ^ A declaration that could also go on a @let@ or @where@ group
     | ClassRelDecl l (classreldecl id l)
     -- ^ A declaration related to the class mechanism
     | DeclExt     (declext id l)
    -- ^ An extension to declarations
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A declaration of a type
data TypeDecl asst ty tydeclext id l
     = TypeDecl    l (DeclHead id l) (ty id l)
     -- ^ A type declaration
     | DataDecl    l (DataOrNew l) (Maybe (Context asst id l)) (DeclHead id l) [QualConDecl asst ty id l] (Maybe (Deriving ty id l))
     -- ^ A data OR newtype declaration
     | GDataDecl   l (DataOrNew l) (Maybe (Context asst id l)) (DeclHead id l) (Maybe (Kind id l)) [GadtDecl ty id l]    (Maybe (Deriving ty id l))
     -- ^ A data OR newtype declaration, ADT style
     | DefaultDecl l [ty id l]
     -- ^ A declaration of default types
     | TypeDeclExt (tydeclext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A flag stating whether a declaration is a data or newtype declaration.
data DataOrNew l = DataType l | NewType l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The head of a type or class declaration.
data DeclHead id l
    = DHead l (Name id l) [TyVarBind id l]
    | DHInfix l (TyVarBind id l) (Name id l) (TyVarBind id l)
    | DHParen l (DeclHead id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)



-- | The head of an instance declaration.
data InstHead ty id l
    = IHead l (QName id l) [ty id l]
    | IHInfix l (ty id l) (QName id l) (ty id l)
    | IHParen l (InstHead ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A deriving clause following a data type declaration.
data Deriving ty id l = Deriving l [InstHead ty id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A binding group inside a @let@ or @where@ clause.
data Binds ty guard exp pat bindext id l
    = BDecls  l [Bind ty guard exp pat bindext id l]     -- ^ An ordinary binding group
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data Bind ty guard exp pat bindext id l
     = FunBind l [Match ty guard exp pat bindext id l]
     -- ^ A set of function binding clauses
     | PatBind l (pat id l) (Maybe (ty id l)) (Rhs guard exp id l) {-where-} (Maybe (Binds ty guard exp pat bindext id l))
     -- ^ A pattern binding
     | TypeSig l [Name id l] (ty id l)
     -- ^ A type signature declaration
     | InfixDecl   l (Assoc l) (Maybe Int) [Op id l]
     -- ^ A declaration of operator fixity
     | BindExt (bindext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Clauses of a function binding.
data Match ty guard exp pat bindext id l
     = Match l      (Name id l) [pat id l]         (Rhs guard exp id l) {-where-} (Maybe (Binds ty guard exp pat bindext id l))
        -- ^ A clause defined with prefix notation, i.e. the function name
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
     | InfixMatch l (pat id l) (Name id l) [pat id l] (Rhs guard exp id l) {-where-} (Maybe (Binds ty guard exp pat bindext id l))
        -- ^ A clause defined with infix notation, i.e. first its first argument
        --  pattern, then the function name, then its following argument(s),
        --  the right-hand side and an optional where clause.
        --  Note that there can be more than two arguments to a function declared
        --  infix, hence the list of pattern arguments.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data QualConDecl asst ty id l
    = QualConDecl l
        {-forall-} (Maybe [TyVarBind id l]) {- . -} (Maybe (Context asst id l))
        {- => -} (ConDecl ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declaration of an ordinary data constructor.
data ConDecl ty id l
     = ConDecl l (Name id l) [BangType ty id l]
                -- ^ ordinary data constructor
     | InfixConDecl l (BangType ty id l) (Name id l) (BangType ty id l)
                -- ^ infix data constructor
     | RecDecl l (Name id l) [FieldDecl ty id l]
                -- ^ record constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declaration of a (list of) named field(s).
data FieldDecl ty id l = FieldDecl l [Name id l] (BangType ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A single constructor declaration in a GADT data type declaration.
data GadtDecl ty id l
    = GadtDecl l (Name id l) (ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

data ClassRelatedDecl asst ty bind classbodyext instbodyext classrelext id l
     = ClassDecl l (Maybe (Context asst id l)) (DeclHead id l) [FunDep id l] (Maybe [ClassBody bind classbodyext id l])
     -- ^ A declaration of a type class
     | InstDecl  l (Maybe (Context asst id l)) (InstHead ty id l) (Maybe [InstBody bind instbodyext id l])
     -- ^ An declaration of a type class instance
     | ClassRelatedExt (classrelext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | Declarations inside a class declaration.
data ClassBody bind classbodyext id l
    = ClsDecl    l (bind id l)
     -- ^ ordinary declaration
    | ClassBodyExt (classbodyext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declarations inside an instance declaration.
data InstBody bind instbodyext id l
    = InsDecl     l (bind id l)
    -- ^ ordinary declaration
    | InsBodyExt  (instbodyext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data BangType ty id l
     = BangedTy   l (ty id l) -- ^ strict component, marked with \"@!@\"
     | UnBangedTy l (ty id l) -- ^ non-strict component
     | UnpackedTy l (ty id l) -- ^ unboxed component, marked with an UNPACK pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The right hand side of a function or pattern binding.
data Rhs guard exp id l
     = UnGuardedRhs l (exp id l) -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  l [GuardedRhs guard exp id l]
                                 -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A guarded right hand side @|@ /stmts/ @=@ /exp/.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
--  NB. @guard@ can be an expression, for H98, or a list of statements, for H2010
data GuardedRhs guard exp id l
     = GuardedRhs l (guard id l) (exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A simple type
data Type tyext id l
     = TyFun   l (Type tyext id l)
                 (Type tyext id l) -- ^ function type
     | TyApp   l (Type tyext id l)
                 (Type tyext id l) -- ^ application of a type constructor
     | TyVar   l (Name id l)            -- ^ type variable
     | TyCon   l (QName id l)           -- ^ named type or type constructor
     | TyExt (tyext id l)                -- ^ an extended type
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A (perhaps) qualified type
data QualType asst ty id l
    = TyForall l
        (Maybe [TyVarBind id l])
        (Maybe (Context asst id l))
        (ty id l)          -- ^ qualified type
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | Flag denoting whether a tuple is boxed or unboxed.
data Boxed = Boxed | Unboxed
  deriving (Eq,Ord,Show,Typeable,Data)

-- | A type variable declaration, optionally with an explicit kind annotation.
data TyVarBind id l
    = KindedVar   l (Name id l) (Kind id l)  -- ^ variable binding with kind annotation
    | UnkindedVar l (Name id l)           -- ^ ordinary variable binding
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An explicit kind annotation.
data Kind id l
    = KindStar  l                    -- ^ @*@, the kind of types
    | KindBang  l                    -- ^ @!@, the kind of unboxed types
    | KindFn    l (Kind id l) (Kind id l)  -- ^ @->@, the kind of a type constructor
    | KindParen l (Kind id l)           -- ^ a parenthesised kind
    | KindVar   l (Name id l)          -- ^ @k@, a kind variable (-XPolyKinds)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data FunDep id l
    = FunDep l [Name id l] [Name id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A context is a set of assertions
data Context asst id l
    = CxSingle l (asst id l)
    | CxTuple  l [asst id l]
    | CxParen  l (Context asst id l)
    | CxEmpty  l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters.
data Asst ty asstext id l
        = ClassA l (QName id l) [ty id l]            -- ^ ordinary class assertion
        | AsstExt (asstext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data Literal l
    = Char       l Char     String -- ^ character literal
    | String     l String   String -- ^ string literal
    | Int        l Integer  String -- ^ integer literal
    | Frac       l Rational String -- ^ floating point literal
    | PrimInt    l Integer  String -- ^ unboxed integer literal
    | PrimWord   l Integer  String -- ^ unboxed word literal
    | PrimFloat  l Rational String -- ^ unboxed float literal
    | PrimDouble l Rational String -- ^ unboxed double literal
    | PrimChar   l Char     String -- ^ unboxed character literal
    | PrimString l String   String -- ^ unboxed string literal
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Haskell expressions.
data Exp binds pat lit expext id l
    = Var l (QName id l)                       -- ^ variable
    | Con l (QName id l)                       -- ^ data constructor
    | Lit l (lit  l)                           -- ^ literal constant
    | App l (Exp binds pat lit expext id l)
            (Exp binds pat lit expext id l)    -- ^ ordinary application
    | Lambda l [pat id l]
               (Exp binds pat lit expext id l) -- ^ lambda expression
    | Let l  (binds id l)
             (Exp binds pat lit expext id l)   -- ^ local declarations with @let@ ... @in@ ...
    | Case l (Exp binds pat lit expext id l)
             [Alt binds pat lit expext id l]   -- ^ @case@ /exp/ @of@ /alts/
    | ExpExt (expext id l)                     -- ^ an extended expression
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A simple alternative in a case expression
data Alt binds pat lit expext id l
   =  Alt l (pat id l) (Exp binds pat lit expext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A top level options pragma, preceding the module header.
data ModulePragma mpragext id l
    = LanguagePragma   l [Name id l]  -- ^ LANGUAGE pragma
    | OptionsPragma    l (Maybe Tool) String
                        -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
    | ModulePragmaExt (mpragext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Recognised tools for OPTIONS pragmas.
data Tool = GHC | HUGS | NHC98 | YHC | HADDOCK | UnknownTool String
  deriving (Eq,Ord,Show,Typeable,Data)

-- | A pattern, to be matched against a value.
data Pat patext id l
    = PVar l (Name id l)                      -- ^ variable
    | PApp l (QName id l) [Pat patext id l]  -- ^ data constructor and argument patterns
    | PWildCard l                              -- ^ wildcard pattern: @_@
    | PExt (patext id l)                       -- ^ an extended pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- General auxiliary types
data OneOrMore t id l
    = OneOrMore (t id l) [t id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: l -> ModuleName String l
prelude_mod l = ModuleName l "Prelude"
main_mod    l = ModuleName l "Main"

main_name :: l -> Name String l
main_name l = Ident l "main"

unit_con_name :: l -> QName id l
unit_con_name l = Special l (UnitCon l)

tuple_con_name :: l -> Boxed -> Int -> QName id l
tuple_con_name l b i = Special l (TupleCon l b (i+1))

list_cons_name :: l -> QName id l
list_cons_name l = Special l (Cons l)

unboxed_singleton_con_name :: l -> QName id l
unboxed_singleton_con_name l = Special l (UnboxedSingleCon l)

unit_con :: l -> Exp binds pat lit expext id l
unit_con l = Con l $ unit_con_name l

tuple_con :: l -> Boxed -> Int -> Exp binds pat lit expext id l
tuple_con l b i = Con l (tuple_con_name l b i)

unboxed_singleton_con :: l -> Exp binds pat lit expext id l
unboxed_singleton_con l = Con l (unboxed_singleton_con_name l)

as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name :: l -> Name String l
as_name        l = Ident  l "as"
qualified_name l = Ident  l "qualified"
hiding_name    l = Ident  l "hiding"
minus_name     l = Symbol l "-"
bang_name      l = Symbol l "!"
dot_name       l = Symbol l "."
star_name      l = Symbol l "*"

export_name, safe_name, unsafe_name, threadsafe_name,
  stdcall_name, ccall_name, cplusplus_name, dotnet_name,
  jvm_name, js_name, forall_name, family_name :: l -> Name String l
export_name     l = Ident l "export"
safe_name       l = Ident l "safe"
unsafe_name     l = Ident l "unsafe"
threadsafe_name l = Ident l "threadsafe"
stdcall_name    l = Ident l "stdcall"
ccall_name      l = Ident l "ccall"
cplusplus_name  l = Ident l "cplusplus"
dotnet_name     l = Ident l "dotnet"
jvm_name        l = Ident l "jvm"
js_name         l = Ident l "js"
forall_name     l = Ident l "forall"
family_name     l = Ident l "family"

unit_tycon_name, fun_tycon_name, list_tycon_name, unboxed_singleton_tycon_name :: l -> QName id l
unit_tycon_name l = unit_con_name l
fun_tycon_name  l = Special l (FunCon l)
list_tycon_name l = Special l (ListCon l)
unboxed_singleton_tycon_name l = Special l (UnboxedSingleCon l)

tuple_tycon_name :: l -> Boxed -> Int -> QName id l
tuple_tycon_name l b i = tuple_con_name l b i

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: l -> Type tyext id l
unit_tycon l = TyCon l $ unit_tycon_name l
fun_tycon  l = TyCon l $ fun_tycon_name  l
list_tycon l = TyCon l $ list_tycon_name l
unboxed_singleton_tycon l = TyCon l $ unboxed_singleton_tycon_name l

tuple_tycon :: l -> Boxed -> Int -> Type tyext id l
tuple_tycon l b i = TyCon l (tuple_tycon_name l b i)


-----------------------------------------------------------------------------
-- AST traversal, boiler-plate style

-- | Test if two AST elements are equal modulo annotations.
(=~=) :: (Functor a, Eq (a ())) => a l1 -> a l2 -> Bool
a =~= b = fmap (const ()) a == fmap (const ()) b

-----------------------------------------------------------------------------
-- Reading annotations

-- | AST nodes are annotated, and this class allows manipulation of the annotations.
class Annotated ast where
    -- | Retrieve the annotation of an AST node.
    ann :: ast l -> l


instance Annotated (ModuleName id) where
    ann (ModuleName l _) = l

instance Annotated SpecialCon where
    ann sc = case sc of
        UnitCon l   -> l
        ListCon l   -> l
        FunCon  l   -> l
        TupleCon l _ _  -> l
        Cons l      -> l
        UnboxedSingleCon l  -> l

instance Annotated (QName id) where
    ann qn = case qn of
        Qual    l _ _  -> l
        UnQual  l    _  -> l
        Special l _    -> l

instance Annotated (Name id) where
    ann (Ident  l _) = l
    ann (Symbol l _) = l

instance Annotated (Op id) where
    ann (VarOp l _) = l
    ann (ConOp l _) = l

instance Annotated (CName id) where
    ann (VarName l _) = l
    ann (ConName l _) = l

instance Annotated (Module bind tydecl classreldecl declext mpragext id) where
    ann (Module l _ _ _ _) = l


instance Annotated (ModuleHead id) where
    ann (ModuleHead l _ _ _) = l

instance Annotated WarningText where
    ann (DeprText l _) = l
    ann (WarnText l _) = l

instance Annotated (ExportSpecList id) where
    ann (ExportSpecList l _) = l

instance Annotated (ExportSpec id) where
    ann es = case es of
        EVar l _       -> l
        EAbs l _       -> l
        EThingAll l _  -> l
        EThingWith l _ _ -> l
        EModuleContents l _    -> l

instance Annotated (ImportDecl id) where
    ann (ImportDecl l _ _ _ _ _ _) = l

instance Annotated (ImportSpecList id) where
    ann (ImportSpecList l _ _) = l

instance Annotated (ImportSpec id) where
    ann is = case is of
        IVar l _        -> l
        IAbs l _        -> l
        IThingAll l _   -> l
        IThingWith l _ _  -> l

instance Annotated Assoc where
    ann (AssocNone  l) = l
    ann (AssocLeft  l) = l
    ann (AssocRight l) = l

instance Annotated (Deriving ty id) where
    ann (Deriving l _)    = l

instance Annotated (declext id)  => Annotated (Decl bind tydecl classreldecl declext id) where
    ann decl = case decl of
        ClassRelDecl l _ -> l
        TyDecl       l _ -> l
        BindDecl     l _ -> l
        DeclExt      de  -> ann de

instance Annotated (tydeclext id) => Annotated (TypeDecl asst ty tydeclext id) where
    ann decl = case decl of
        TypeDecl    l _ _         -> l
        DataDecl    l _ _ _ _ _   -> l
        GDataDecl   l _ _ _ _ _ _ -> l
        DefaultDecl l _           -> l
        TypeDeclExt tydeclext     -> ann tydeclext

instance Annotated DataOrNew where
    ann (DataType l) = l
    ann (NewType  l) = l

instance Annotated (DeclHead id) where
    ann (DHead l _ _)       = l
    ann (DHInfix l _ _ _) = l
    ann (DHParen l _)        = l

instance Annotated (InstHead ty id) where
    ann (IHead l _ _) = l
    ann (IHInfix l _ _ _) = l
    ann (IHParen l _) = l

instance Annotated (Binds ty guard exp pat bindext id) where
    ann (BDecls  l _) = l

instance Annotated (bindext id) => Annotated  (Bind ty guard exp pat bindext id) where
    ann b = case b of
        FunBind   l _       -> l
        PatBind   l _ _ _ _ -> l
        TypeSig   l _ _     -> l
        InfixDecl l _ _ _   -> l
        BindExt bext        -> ann bext

instance Annotated (Match ty guard exp pat bindext id) where
    ann (Match      l _ _ _ _)   = l
    ann (InfixMatch l _ _ _ _ _) = l

instance Annotated (QualConDecl asst ty id) where
    ann (QualConDecl l _ _ _) = l

instance Annotated (ConDecl ty id) where
    ann (ConDecl l _ _) = l
    ann (InfixConDecl l _ _ _) = l
    ann (RecDecl l _ _) = l

instance Annotated (FieldDecl ty id) where
    ann (FieldDecl l _ _) = l

instance Annotated (GadtDecl ty id) where
    ann (GadtDecl l _ _) = l

instance Annotated (classrelext id) => Annotated (ClassRelatedDecl asst ty bind classbodyext instbodyext classrelext id) where
    ann cr = case cr of
        ClassDecl l _ _ _ _ -> l
        InstDecl  l _ _ _   -> l
        ClassRelatedExt ce  -> ann ce

instance Annotated (classbodyext id) => Annotated (ClassBody bind classbodyext id) where
    ann (ClsDecl    l _) = l
    ann (ClassBodyExt e) = ann e

instance Annotated (instbodyext id) => Annotated (InstBody bind instbodyext id) where
    ann insd = case insd of
        InsDecl    l _ -> l
        InsBodyExt e   -> ann e

instance Annotated (BangType ty id) where
     ann (BangedTy   l _) = l
     ann (UnBangedTy l _) = l
     ann (UnpackedTy l _) = l

instance Annotated (Rhs guard exp id) where
     ann (UnGuardedRhs l _) = l
     ann (GuardedRhss  l _) = l

instance Annotated (GuardedRhs guard exp id) where
     ann (GuardedRhs l _ _) = l

instance Annotated (tyext id) => Annotated (Type tyext id) where
    ann t = case t of
      TyFun   l _ _    -> l
      TyApp   l _ _    -> l
      TyVar   l _      -> l
      TyCon   l _      -> l
      TyExt   tyext    -> ann tyext

instance Annotated (QualType asst ty id) where
    ann (TyForall l _ _ _) = l

instance Annotated (TyVarBind id) where
    ann (KindedVar   l _ _) = l
    ann (UnkindedVar l _)   = l

instance Annotated (Kind id) where
    ann (KindStar l) = l
    ann (KindBang l) = l
    ann (KindFn   l _ _) = l
    ann (KindParen l _) = l
    ann (KindVar l _) = l

instance Annotated (FunDep id) where
    ann (FunDep l _ _) = l

instance Annotated (Context asst id) where
    ann (CxSingle l _ ) = l
    ann (CxTuple  l _) = l
    ann (CxParen  l _ )  = l
    ann (CxEmpty  l)       = l

instance Annotated (asstext id) => Annotated (Asst ty asstext id) where
    ann asst = case asst of
        ClassA l _ _ -> l
        AsstExt a    -> ann a

instance Annotated Literal where
    ann lit = case lit of
        Char       l _ _ -> l
        String     l _ _ -> l
        Int        l _ _ -> l
        Frac       l _ _ -> l
        PrimInt    l _ _ -> l
        PrimWord   l _ _ -> l
        PrimFloat  l _ _ -> l
        PrimDouble l _ _ -> l
        PrimChar   l _ _ -> l
        PrimString l _ _ -> l

instance Annotated (expext id) => Annotated (Exp binds pat lit expext id) where
    ann e = case e of
        Var l _        -> l
        Con l _        -> l
        Lit l _       -> l
        App l _ _     -> l
        Lambda l _ _   -> l
        Let l _ _      -> l
        Case l _ _   -> l
        ExpExt extexp -> ann extexp

instance Annotated (Alt binds pat lit expext id) where
    ann (Alt l _ _) = l

instance Annotated (mpragext id) => Annotated (ModulePragma mpragext id) where
    ann (LanguagePragma   l _)   = l
    ann (OptionsPragma    l _ _) = l
    ann (ModulePragmaExt  e)     = ann e

instance Annotated (patext id) => Annotated (Pat patext id) where
    ann p = case p of
      PVar l _     -> l
      PApp l _ _   -> l
      PWildCard l  -> l
      PExt patext  -> ann patext

instance Annotated (t id) => Annotated (OneOrMore t id) where
    ann (OneOrMore a _) = ann a
