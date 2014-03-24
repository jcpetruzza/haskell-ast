{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST

where


import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- | The name of a Haskell module.
data GModuleName id l = ModuleName l id
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.
data GSpecialCon l
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
data GQName id l
    = Qual    l (GModuleName id l) (GName id l)    -- ^ name qualified with a module name
    | UnQual  l                   (GName id l)    -- ^ unqualified local name
    | Special l (GSpecialCon l)  -- ^ built-in constructor with special syntax
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | This type is used to represent variables, and also constructors.
data GName id l
    = Ident  l id   -- ^ /varid/ or /conid/.
    | Symbol l id   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Operators appearing in @infix@ declarations are never qualified.
data GOp id l
    = VarOp l (GName id l)    -- ^ variable operator (/varop/)
    | ConOp l (GName id l)    -- ^ constructor operator (/conop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data GCName id l
    = VarName l (GName id l) -- ^ name of a method or field
    | ConName l (GName id l) -- ^ name of a data constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A complete Haskell source module.
data GModule asst ty guard exp pat lit id l
    = Module l (Maybe (GModuleHead id l)) [GModulePragma id l] [GImportDecl id l] [GDecl asst ty guard exp pat lit id l]
    -- ^ an ordinary Haskell module
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The head of a module, including the name and export specification.
data GModuleHead id l = ModuleHead l (GModuleName id l) (Maybe (GWarningText l)) (Maybe (GExportSpecList id l))
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Warning text to optionally use in the module header of e.g.
--   a deprecated module.
data GWarningText l
    = DeprText l String
    | WarnText l String
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | An explicit export specification. The 'Bool' is 'True' if the export has
-- the @type@ keyword (@-XExplicitNamespaces@)
data GExportSpecList id l
    = ExportSpecList l [GExportSpec id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An item in a module's export specification.
data GExportSpec id l
     = EVar l (GQName id l)                 -- ^ variable
     | EAbs l (GQName id l)                 -- ^ @T@:
                                           --   a class or datatype exported abstractly,
                                           --   or a type synonym.
     | EThingAll l (GQName id l)            -- ^ @T(..)@:
                                           --   a class exported with all of its methods, or
                                           --   a datatype exported with all of its constructors.
     | EThingWith l (GQName id l) [GCName id l]     -- ^ @T(C_1,...,C_n)@:
                                           --   a class exported with some of its methods, or
                                           --   a datatype exported with some of its constructors.
     | EModuleContents l (GModuleName id l) -- ^ @module M@:
                                           --   re-export a module.
     -- | EType id l (GExportSpec id l)           -- ^ @type x@: available with @-XExplicitNamespaces@

  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An import declaration.
data GImportDecl id l = ImportDecl
    { importAnn :: l                             -- ^ annotation, used by parser for position of the @import@ keyword.
    , importModule :: GModuleName id l            -- ^ name of the module imported.
    , importQualified :: Bool                    -- ^ imported @qualified@?
    , importSrc :: Bool                          -- ^ imported with @{-\# SOURCE \#-}@?
    , importPkg :: Maybe String                  -- ^ imported with explicit package name
    , importAs :: Maybe (GModuleName id l)        -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe (GImportSpecList id l)
            -- ^ optional list of import specifications.
    }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An explicit import specification list.
data GImportSpecList id l
    = ImportSpecList l Bool [GImportSpec id l]
            -- A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
            --
            -- The other 'Bool' is true if the 'ImportSpec' has
            -- the @type@ keyword @-XExplicitNamespaces@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data GImportSpec id l
     = IVar l (GName id l)           -- ^ variable
     | IAbs l (GName id l)           -- ^ @T@:
                                    --   the name of a class, datatype or type synonym.
     | IThingAll l (GName id l)      -- ^ @T(..)@:
                                    --   a class imported with all of its methods, or
                                    --   a datatype imported with all of its constructors.
     | IThingWith l (GName id l) [GCName id l]    -- ^ @T(C_1,...,C_n)@:
                                    --   a class imported with some of its methods, or
                                    --   a datatype imported with some of its constructors.
     -- | IType l (GImportSpec id l)    -- ^ @type ...@ (-XExplicitNamespaces)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Associativity of an operator.
data GAssoc l
     = AssocNone  l -- ^ non-associative operator (declared with @infix@)
     | AssocLeft  l -- ^ left-associative operator (declared with @infixl@).
     | AssocRight l -- ^ right-associative operator (declared with @infixr@)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A top-level declaration.
data GDecl asst ty guard exp pat lit id l
     = TypeDecl     l (GDeclHead id l) (ty id l)
     -- ^ A type declaration
     | TypeFamDecl  l (GDeclHead id l) (Maybe (GKind id l))
     -- ^ A type family declaration
     | DataDecl     l (GDataOrNew l) (Maybe (GContext asst id l)) (GDeclHead id l) [GQualConDecl asst ty id l] (Maybe (GDeriving ty id l))
     -- ^ A data OR newtype declaration
     | GDataDecl    l (GDataOrNew l) (Maybe (GContext asst id l)) (GDeclHead id l) (Maybe (GKind id l)) [GGadtDecl ty id l]    (Maybe (GDeriving ty id l))
     -- ^ A data OR newtype declaration, GADT style
     | DataFamDecl  l {-data-}      (Maybe (GContext asst id l)) (GDeclHead id l) (Maybe (GKind id l))
     -- ^ A data family declaration
     | TypeInsDecl  l (ty id l) (ty id l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (GDataOrNew l) (ty id l) [GQualConDecl asst ty id l] (Maybe (GDeriving ty id l))
     -- ^ A data family instance declaration
     | GDataInsDecl l (GDataOrNew l) (ty id l) (Maybe (GKind id l)) [GGadtDecl ty id l]    (Maybe (GDeriving ty id l))
     -- ^ A data family instance declaration, GADT style
     | ClassDecl    l (Maybe (GContext asst id l)) (GDeclHead id l) [GFunDep id l] (Maybe [GClassDecl asst ty guard exp pat lit id l])
     -- ^ A declaration of a type class
     | InstDecl     l (Maybe (GContext asst id l)) (GInstHead ty id l) (Maybe [GInstDecl asst ty guard exp pat lit id l])
     -- ^ An declaration of a type class instance
     | DerivDecl    l (Maybe (GContext asst id l)) (GInstHead ty id l)
     -- ^ A standalone deriving declaration
     | InfixDecl    l (GAssoc l) (Maybe Int) [GOp id l]
     -- ^ A declaration of operator fixity
     | DefaultDecl  l [ty id l]
     -- ^ A declaration of default types
     | TypeSig      l [GName id l] (ty id l)
     -- ^ A type signature declaration
     | FunBind      l [GMatch asst ty guard exp pat lit id l]
     -- ^ A set of function binding clauses
     | PatBind      l (pat id l) (Maybe (ty id l)) (GRhs guard exp id l) {-where-} (Maybe (GBinds asst ty guard exp pat lit id l))
     -- ^ A pattern binding
     | ForImp       l (GCallConv l) (Maybe (GSafety l)) (Maybe String) (GName id l) (ty id l)
     -- ^ A foreign import declaration
     | ForExp       l (GCallConv l)                    (Maybe String) (GName id l) (ty id l)
     -- ^ A foreign export declaration
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A flag stating whether a declaration is a data or newtype declaration.
data GDataOrNew l = DataType l | NewType l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The head of a type or class declaration.
data GDeclHead id l
    = DHead l (GName id l) [GTyVarBind id l]
    | DHInfix l (GTyVarBind id l) (GName id l) (GTyVarBind id l)
    | DHParen l (GDeclHead id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The head of an instance declaration.
data GInstHead ty id l
    = IHead l (GQName id l) [ty id l]
    | IHInfix l (ty id l) (GQName id l) (ty id l)
    | IHParen l (GInstHead ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A deriving clause following a data type declaration.
data GDeriving ty id l = Deriving l [GInstHead ty id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A binding group inside a @let@ or @where@ clause.
data GBinds asst ty guard exp pat lit id l
    = BDecls  l [GDecl asst ty guard exp pat lit id l]     -- ^ An ordinary binding group
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Clauses of a function binding.
data GMatch asst ty guard exp pat lit id l
     = Match l      (GName id l) [pat id l]         (GRhs guard exp id l) {-where-} (Maybe (GBinds asst ty guard exp pat lit id l))
        -- ^ A clause defined with prefix notation, i.e. the function name
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
     | InfixMatch l (pat id l) (GName id l) [pat id l] (GRhs guard exp id l) {-where-} (Maybe (GBinds asst ty guard exp pat lit id l))
        -- ^ A clause defined with infix notation, i.e. first its first argument
        --  pattern, then the function name, then its following argument(s),
        --  the right-hand side and an optional where clause.
        --  Note that there can be more than two arguments to a function declared
        --  infix, hence the list of pattern arguments.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data GQualConDecl asst ty id l
    = QualConDecl l
        {-forall-} (Maybe [GTyVarBind id l]) {- . -} (Maybe (GContext asst id l))
        {- => -} (GConDecl ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declaration of an ordinary data constructor.
data GConDecl ty id l
     = ConDecl l (GName id l) [GBangType ty id l]
                -- ^ ordinary data constructor
     | InfixConDecl l (GBangType ty id l) (GName id l) (GBangType ty id l)
                -- ^ infix data constructor
     | RecDecl l (GName id l) [GFieldDecl ty id l]
                -- ^ record constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declaration of a (list of) named field(s).
data GFieldDecl ty id l = FieldDecl l [GName id l] (GBangType ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A single constructor declaration in a GADT data type declaration.
data GGadtDecl ty id l
    = GadtDecl l (GName id l) (ty id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declarations inside a class declaration.
data GClassDecl asst ty guard exp pat lit id l
    = ClsDecl    l (GDecl asst ty guard exp pat lit id l)
            -- ^ ordinary declaration
    | ClsDataFam l (Maybe (GContext asst id l)) (GDeclHead id l) (Maybe (GKind id l))
            -- ^ declaration of an associated data type
    | ClsTyFam   l (GDeclHead id l) (Maybe (GKind id l))
            -- ^ declaration of an associated type synonym
    | ClsTyDef   l (ty id l) (ty id l)
            -- ^ default choice for an associated type synonym
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declarations inside an instance declaration.
data GInstDecl asst ty guard exp pat lit id l
    = InsDecl   l (GDecl asst ty guard exp pat lit id l)
            -- ^ ordinary declaration
    | InsType   l (ty id l) (ty id l)
            -- ^ an associated type definition
    | InsData   l (GDataOrNew l) (ty id l) [GQualConDecl asst ty id l] (Maybe (GDeriving ty id l))
            -- ^ an associated data type implementation
    | InsGData  l (GDataOrNew l) (ty id l) (Maybe (GKind id l)) [GGadtDecl ty id l] (Maybe (GDeriving ty id l))
            -- ^ an associated data type implemented using GADT style
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data GBangType ty id l
     = BangedTy   l (ty id l) -- ^ strict component, marked with \"@!@\"
     | UnBangedTy l (ty id l) -- ^ non-strict component
     | UnpackedTy l (ty id l) -- ^ unboxed component, marked with an UNPACK pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The right hand side of a function or pattern binding.
data GRhs guard exp id l
     = UnGuardedRhs l (exp id l) -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  l [GGuardedRhs guard exp id l]
                                 -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A guarded right hand side @|@ /stmts/ @=@ /exp/.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
--  NB. @guard@ can be an expression, for H98, or a list of statements, for H2010
data GGuardedRhs guard exp id l
     = GuardedRhs l guard (exp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A simple type
data GType tyext id l
     = TyFun   l (GType tyext id l)
                 (GType tyext id l) -- ^ function type
     | TyApp   l (GType tyext id l)
                 (GType tyext id l) -- ^ application of a type constructor
     | TyVar   l (GName id l)            -- ^ type variable
     | TyCon   l (GQName id l)           -- ^ named type or type constructor
     | TyExt (tyext id l)                -- ^ an extended type
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A (perhaps) qualified type
data GQualType asst ty id l
    = TyForall l
        (Maybe [GTyVarBind id l])
        (Maybe (asst id l))
        (ty id l)          -- ^ qualified type
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | Flag denoting whether a tuple is boxed or unboxed.
data Boxed = Boxed | Unboxed
  deriving (Eq,Ord,Show,Typeable,Data)

-- | A type variable declaration, optionally with an explicit kind annotation.
data GTyVarBind id l
    = KindedVar   l (GName id l) (GKind id l)  -- ^ variable binding with kind annotation
    | UnkindedVar l (GName id l)           -- ^ ordinary variable binding
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An explicit kind annotation.
data GKind id l
    = KindStar  l                    -- ^ @*@, the kind of types
    | KindBang  l                    -- ^ @!@, the kind of unboxed types
    | KindFn    l (GKind id l) (GKind id l)  -- ^ @->@, the kind of a type constructor
    | KindParen l (GKind id l)           -- ^ a parenthesised kind
    | KindVar   l (GName id l)          -- ^ @k@, a kind variable (-XPolyKinds)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data GFunDep id l
    = FunDep l [GName id l] [GName id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A context is a set of assertions
data GContext asst id l
    = CxSingle l (asst id l)
    | CxTuple  l [asst id l]
    | CxParen  l (GContext asst id l)
    | CxEmpty  l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
data GAsst ty asstext id l
        = ClassA l (GQName id l) [ty id l]            -- ^ ordinary class assertion
        | AsstExt (asstext id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data GLiteral l
    = Char       l Char     -- ^ character literal
    | String     l String   -- ^ string literal
    | Int        l Integer  -- ^ integer literal
    | Frac       l Rational -- ^ floating point literal
    | PrimInt    l Integer  -- ^ unboxed integer literal
    | PrimWord   l Integer  -- ^ unboxed word literal
    | PrimFloat  l Rational -- ^ unboxed float literal
    | PrimDouble l Rational -- ^ unboxed double literal
    | PrimChar   l Char     -- ^ unboxed character literal
    | PrimString l String   -- ^ unboxed string literal
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Haskell expressions.
data GExp binds pat lit expext id l
    = Var l (GQName id l)                       -- ^ variable
    | Con l (GQName id l)                       -- ^ data constructor
    | Lit l lit                                 -- ^ literal constant
    | App l (GExp binds pat lit expext id l)
            (GExp binds pat lit expext id l)    -- ^ ordinary application
    | Lambda l [pat id l]
               (GExp binds pat lit expext id l) -- ^ lambda expression
    | Let l  binds
             (GExp binds pat lit expext id l)   -- ^ local declarations with @let@ ... @in@ ...
    | Case l (GExp binds pat lit expext id l)
             (GExp binds pat lit expext id l)   -- ^ @case@ /exp/ @of@ /alts/
    | ExpExt (expext id l)                      -- ^ an extended expression
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The safety of a foreign function call.
data GSafety l
    = PlayRisky l         -- ^ unsafe
    | PlaySafe l Bool     -- ^ safe ('False') or threadsafe ('True')
    | PlayInterruptible l -- ^ interruptible
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The calling convention of a foreign function call.
data GCallConv l
    = StdCall l
    | CCall l
    | CPlusPlus l
    | DotNet l
    | Jvm l
    | Js l
    | CApi l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A top level options pragma, preceding the module header.
data GModulePragma id l
    = LanguagePragma   l [GName id l]  -- ^ LANGUAGE pragma
    | OptionsPragma    l (Maybe Tool) String
                        -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Recognised tools for OPTIONS pragmas.
data Tool = GHC | HUGS | NHC98 | YHC | HADDOCK | UnknownTool String
  deriving (Eq,Ord,Show,Typeable,Data)

-- | A pattern, to be matched against a value.
data GPat patext id l
    = PVar l (GName id l)                      -- ^ variable
    | PApp l (GQName id l) [GPat patext id l]  -- ^ data constructor and argument patterns
    | PWildCard l                              -- ^ wildcard pattern: @_@
    | PExt (patext id l)                       -- ^ an extended pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)



-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: l -> GModuleName String l
prelude_mod l = ModuleName l "Prelude"
main_mod    l = ModuleName l "Main"

main_name :: l -> GName String l
main_name l = Ident l "main"

unit_con_name :: l -> GQName id l
unit_con_name l = Special l (UnitCon l)

tuple_con_name :: l -> Boxed -> Int -> GQName id l
tuple_con_name l b i = Special l (TupleCon l b (i+1))

list_cons_name :: l -> GQName id l
list_cons_name l = Special l (Cons l)

unboxed_singleton_con_name :: l -> GQName id l
unboxed_singleton_con_name l = Special l (UnboxedSingleCon l)

unit_con :: l -> GExp binds pat lit expext id l
unit_con l = Con l $ unit_con_name l

tuple_con :: l -> Boxed -> Int -> GExp binds pat lit expext id l
tuple_con l b i = Con l (tuple_con_name l b i)

unboxed_singleton_con :: l -> GExp binds pat lit expext id l
unboxed_singleton_con l = Con l (unboxed_singleton_con_name l)

as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name :: l -> GName String l
as_name        l = Ident  l "as"
qualified_name l = Ident  l "qualified"
hiding_name    l = Ident  l "hiding"
minus_name     l = Symbol l "-"
bang_name      l = Symbol l "!"
dot_name       l = Symbol l "."
star_name      l = Symbol l "*"

export_name, safe_name, unsafe_name, threadsafe_name,
  stdcall_name, ccall_name, cplusplus_name, dotnet_name,
  jvm_name, js_name, forall_name, family_name :: l -> GName String l
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

unit_tycon_name, fun_tycon_name, list_tycon_name, unboxed_singleton_tycon_name :: l -> GQName id l
unit_tycon_name l = unit_con_name l
fun_tycon_name  l = Special l (FunCon l)
list_tycon_name l = Special l (ListCon l)
unboxed_singleton_tycon_name l = Special l (UnboxedSingleCon l)

tuple_tycon_name :: l -> Boxed -> Int -> GQName id l
tuple_tycon_name l b i = tuple_con_name l b i

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: l -> GType tyext id l
unit_tycon l = TyCon l $ unit_tycon_name l
fun_tycon  l = TyCon l $ fun_tycon_name  l
list_tycon l = TyCon l $ list_tycon_name l
unboxed_singleton_tycon l = TyCon l $ unboxed_singleton_tycon_name l

tuple_tycon :: l -> Boxed -> Int -> GType tyext id l
tuple_tycon l b i = TyCon l (tuple_tycon_name l b i)


-----------------------------------------------------------------------------
-- AST traversal, boiler-plate style

-- | Test if two AST elements are equal modulo annotations.
(=~=) :: (Annotated a, Eq (a ())) => a l1 -> a l2 -> Bool
a =~= b = fmap (const ()) a == fmap (const ()) b

-----------------------------------------------------------------------------
-- Reading annotations

-- | AST nodes are annotated, and this class allows manipulation of the annotations.
class Functor ast => Annotated ast where
    -- | Retrieve the annotation of an AST node.
    ann :: ast l -> l
    -- | Change the annotation of an AST node. Note that only the annotation of
    --   the node itself is affected, and not the annotations of any child nodes.
    --   if all nodes in the AST tree are to be affected, use 'fmap'.
    amap :: (l -> l) -> ast l -> ast l

instance Annotated (GModuleName id) where
    ann (ModuleName l _) = l
    amap = fmap

instance Annotated GSpecialCon where
    ann sc = case sc of
        UnitCon l   -> l
        ListCon l   -> l
        FunCon  l   -> l
        TupleCon l _ _  -> l
        Cons l      -> l
        UnboxedSingleCon l  -> l
    amap = fmap

instance Annotated (GQName id) where
    ann qn = case qn of
        Qual    l _ _  -> l
        UnQual  l    _  -> l
        Special l _    -> l
    amap = fmap

instance Annotated (GName id) where
    ann (Ident  l _) = l
    ann (Symbol l _) = l
    amap = fmap

instance Annotated (GOp id) where
    ann (VarOp l _) = l
    ann (ConOp l _) = l
    amap = fmap

instance Annotated (GCName id) where
    ann (VarName l _) = l
    ann (ConName l _) = l
    amap = fmap

instance (Functor (asst id), Functor (ty id), Functor (exp id), Functor (pat id))
 => Annotated (GModule asst ty guard exp pat lit id) where
    ann (Module l _ _ _ _) = l
    amap = fmap


instance Annotated (GModuleHead id) where
    ann (ModuleHead l _ _ _) = l
    amap = fmap

instance Annotated GWarningText where
    ann (DeprText l _) = l
    ann (WarnText l _) = l
    amap = fmap

instance Annotated (GExportSpecList id) where
    ann (ExportSpecList l _) = l
    amap = fmap

instance Annotated (GExportSpec id) where
    ann es = case es of
        EVar l _       -> l
        EAbs l _       -> l
        EThingAll l _  -> l
        EThingWith l _ _ -> l
        EModuleContents l _    -> l
    amap = fmap

instance Annotated (GImportDecl id) where
    ann (ImportDecl l _ _ _ _ _ _) = l
    amap = fmap

instance Annotated (GImportSpecList id) where
    ann (ImportSpecList l _ _) = l
    amap = fmap

instance Annotated (GImportSpec id) where
    ann is = case is of
        IVar l _        -> l
        IAbs l _        -> l
        IThingAll l _   -> l
        IThingWith l _ _  -> l
    amap = fmap

instance Annotated GAssoc where
    ann (AssocNone  l) = l
    ann (AssocLeft  l) = l
    ann (AssocRight l) = l
    amap = fmap

instance Functor (ty id) => Annotated (GDeriving ty id) where
    ann (Deriving l _)    = l
    amap = fmap

instance (Functor (asst id), Functor (ty id), Functor (exp id), Functor (pat id))
 => Annotated (GDecl asst ty guard exp pat lit id) where
    ann decl = case decl of
        TypeDecl     l _ _         -> l
        TypeFamDecl  l _ _         -> l
        DataDecl     l _ _ _ _ _   -> l
        GDataDecl    l _ _ _ _ _ _ -> l
        DataFamDecl  l _ _ _       -> l
        TypeInsDecl  l _ _         -> l
        DataInsDecl  l _ _ _ _     -> l
        GDataInsDecl l _ _ _ _ _   -> l
        ClassDecl    l _ _ _ _     -> l
        InstDecl     l _ _ _       -> l
        DerivDecl    l _ _         -> l
        InfixDecl    l _ _ _       -> l
        DefaultDecl  l _           -> l
        TypeSig      l _ _         -> l
        FunBind      l _           -> l
        PatBind      l _ _ _ _     -> l
        ForImp       l _ _ _ _ _   -> l
        ForExp       l _ _ _ _     -> l

    amap = fmap

instance Annotated GDataOrNew where
    ann (DataType l) = l
    ann (NewType  l) = l
    amap = fmap

instance Annotated (GDeclHead id) where
    ann (DHead l _ _)       = l
    ann (DHInfix l _ _ _) = l
    ann (DHParen l _)        = l
    amap = fmap

instance Functor (ty id) => Annotated (GInstHead ty id) where
    ann (IHead l _ _) = l
    ann (IHInfix l _ _ _) = l
    ann (IHParen l _) = l
    amap = fmap

instance (Functor (asst id), Functor (ty id), Functor (exp id), Functor (pat id))
 => Annotated (GBinds asst ty guard exp pat lit id) where
    ann (BDecls  l _) = l
    amap = fmap

instance (Functor (asst id), Functor (ty id), Functor (exp id), Functor (pat id))
 => Annotated (GMatch asst ty guard exp pat lit id) where
    ann (Match l _ _ _ _) = l
    ann (InfixMatch l _ _ _ _ _) = l
    amap = fmap

instance (Functor (asst id), Functor (ty id))
 => Annotated (GQualConDecl asst ty id) where
    ann (QualConDecl l _ _ _) = l
    amap = fmap

instance Functor (ty id) => Annotated (GConDecl ty id) where
    ann (ConDecl l _ _) = l
    ann (InfixConDecl l _ _ _) = l
    ann (RecDecl l _ _) = l
    amap = fmap

instance Functor (ty id) => Annotated (GFieldDecl ty id) where
    ann (FieldDecl l _ _) = l
    amap = fmap

instance Functor (ty id) => Annotated (GGadtDecl ty id) where
    ann (GadtDecl l _ _) = l
    amap = fmap

instance (Functor (asst id), Functor (ty id), Functor (exp id), Functor (pat id))
 => Annotated (GClassDecl asst ty guard exp pat lit id) where
    ann (ClsDecl    l _) = l
    ann (ClsDataFam l _ _ _) = l
    ann (ClsTyFam   l    _ _) = l
    ann (ClsTyDef   l _ _) = l
    amap = fmap

instance (Functor (asst id), Functor (ty id), Functor (exp id), Functor (pat id))
 => Annotated (GInstDecl asst ty guard exp pat lit id) where
    ann insd = case insd of
        InsDecl   l _          -> l
        InsType   l _ _        -> l
        InsData   l _ _ _ _    -> l
        InsGData  l _ _ _ _ _  -> l
    amap = fmap

instance Functor (ty id) => Annotated (GBangType ty id) where
     ann (BangedTy   l _) = l
     ann (UnBangedTy l _) = l
     ann (UnpackedTy l _) = l
     amap = fmap

instance Functor (exp id) => Annotated (GRhs guard exp id) where
     ann (UnGuardedRhs l _) = l
     ann (GuardedRhss  l _) = l
     amap = fmap

instance Functor (exp id) => Annotated (GGuardedRhs guard exp id) where
     ann (GuardedRhs l _ _) = l
     amap = fmap

instance Functor (tyext id) => Annotated (GType tyext id) where
    ann t = case t of
      TyFun   l _ _    -> l
      TyApp   l _ _    -> l
      TyVar   l _      -> l
      TyCon   l _      -> l
    amap = fmap

instance (Functor (asst id), Functor (ty id))
 => Annotated (GQualType asst ty id) where
    ann (TyForall l _ _ _) = l
    amap = fmap

instance Annotated (GTyVarBind id) where
    ann (KindedVar   l _ _) = l
    ann (UnkindedVar l _)   = l
    amap = fmap

instance Annotated (GKind id) where
    ann (KindStar l) = l
    ann (KindBang l) = l
    ann (KindFn   l _ _) = l
    ann (KindParen l _) = l
    ann (KindVar l _) = l
    amap = fmap

instance Annotated (GFunDep id) where
    ann (FunDep l _ _) = l
    amap = fmap

instance Functor (asst id) => Annotated (GContext asst id) where
    ann (CxSingle l _ ) = l
    ann (CxTuple  l _) = l
    ann (CxParen  l _ )  = l
    ann (CxEmpty  l)       = l
    amap = fmap

instance (Functor (ty id), Annotated (asstext id))
 => Annotated (GAsst ty asstext id) where
    ann asst = case asst of
        ClassA l _ _ -> l
        AsstExt a    -> ann a
    amap = fmap

instance Annotated GLiteral where
    ann lit = case lit of
        Char    l _    -> l
        String  l _    -> l
        Int     l _    -> l
        Frac    l _    -> l
        PrimInt    l _ -> l
        PrimWord   l _ -> l
        PrimFloat  l _ -> l
        PrimDouble l _ -> l
        PrimChar   l _ -> l
        PrimString l _ -> l
    amap = fmap

instance (Functor (pat id), Annotated (expext id))
 => Annotated (GExp binds pat lit expext id) where
    ann e = case e of
        Var l _        -> l
        Con l _        -> l
        Lit l _       -> l
        App l _ _     -> l
        Lambda l _ _   -> l
        Let l _ _      -> l
        Case l _ _   -> l
        ExpExt extexp -> ann extexp
    amap = fmap

instance Annotated GSafety where
    ann (PlayRisky l) = l
    ann (PlaySafe l _) = l
    ann (PlayInterruptible l) = l
    amap = fmap

instance Annotated GCallConv where
    ann (StdCall l) = l
    ann (CCall l) = l
    ann (CPlusPlus l) = l
    ann (DotNet l) = l
    ann (Jvm l) = l
    ann (Js l) = l
    ann (CApi l) = l
    amap = fmap

instance Annotated (GModulePragma id) where
    ann (LanguagePragma   l _) = l
    ann (OptionsPragma    l _ _) = l
    amap = fmap

instance Annotated (patext id) => Annotated (GPat patext id) where
    ann p = case p of
      PVar l _     -> l
      PApp l _ _   -> l
      PWildCard l  -> l
      PExt patext  -> ann patext
    amap = fmap

