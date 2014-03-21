{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, DeriveGeneric #-}
module Language.Haskell.AST

 where


import Data.Data
import Data.Generics (Data(..),Typeable(..))

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

-- | An implicit parameter name.
data GIPName id l
    = IPDup l id -- ^ ?/ident/, non-linear implicit parameter
    | IPLin l id -- ^ %/ident/, linear implicit parameter
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data GQOp id l
    = QVarOp l (GQName id l) -- ^ variable operator (/qvarop/)
    | QConOp l (GQName id l) -- ^ constructor operator (/qconop/)
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
data GModule id l
    = Module l (Maybe (GModuleHead id l)) [GModulePragma id l] [GImportDecl id l] [GDecl id l]
    -- ^ an ordinary Haskell module
    | XmlPage l (GModuleName id l) [GModulePragma id l] (GXName id l) [GXAttr id l] (Maybe (GExp id l)) [GExp id l]
    -- ^ a module consisting of a single XML document. The ModuleName never appears in the source
    --   but is needed for semantic purposes, it will be the same as the file name.
    | XmlHybrid l (Maybe (GModuleHead id l)) [GModulePragma id l] [GImportDecl id l] [GDecl id l]
                (GXName id l) [GXAttr id l] (Maybe (GExp id l)) [GExp id l]
    -- ^ a hybrid module combining an XML document with an ordinary module
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The head of a module, including the name and export specification.
data GModuleHead id l = ModuleHead l (GModuleName id l) (Maybe (GWarningText l)) (Maybe (GExportSpecList id l))
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
data GDecl id l
     = TypeDecl     l (GDeclHead id l) (GType id l)
     -- ^ A type declaration
     | TypeFamDecl  l (GDeclHead id l) (Maybe (GKind id l))
     -- ^ A type family declaration
     | DataDecl     l (GDataOrNew l) (Maybe (GContext id l)) (GDeclHead id l)                  [GQualConDecl id l] (Maybe (GDeriving id l))
     -- ^ A data OR newtype declaration
     | GDataDecl    l (GDataOrNew l) (Maybe (GContext id l)) (GDeclHead id l) (Maybe (GKind id l)) [GGadtDecl id l]    (Maybe (GDeriving id l))
     -- ^ A data OR newtype declaration, GADT style
     | DataFamDecl  l {-data-}      (Maybe (GContext id l)) (GDeclHead id l) (Maybe (GKind id l))
     -- ^ A data family declaration
     | TypeInsDecl  l (GType id l) (GType id l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (GDataOrNew l) (GType id l)                  [GQualConDecl id l] (Maybe (GDeriving id l))
     -- ^ A data family instance declaration
     | GDataInsDecl l (GDataOrNew l) (GType id l) (Maybe (GKind id l)) [GGadtDecl id l]    (Maybe (GDeriving id l))
     -- ^ A data family instance declaration, GADT style
     | ClassDecl    l (Maybe (GContext id l)) (GDeclHead id l) [GFunDep id l] (Maybe [GClassDecl id l])
     -- ^ A declaration of a type class
     | InstDecl     l (Maybe (GContext id l)) (GInstHead id l) (Maybe [GInstDecl id l])
     -- ^ An declaration of a type class instance
     | DerivDecl    l (Maybe (GContext id l)) (GInstHead id l)
     -- ^ A standalone deriving declaration
     | InfixDecl    l (GAssoc l) (Maybe Int) [GOp id l]
     -- ^ A declaration of operator fixity
     | DefaultDecl  l [GType id l]
     -- ^ A declaration of default types
     | SpliceDecl   l (GExp id l)
     -- ^ A Template Haskell splicing declaration
     | TypeSig      l [GName id l] (GType id l)
     -- ^ A type signature declaration
     | FunBind      l [GMatch id l]
     -- ^ A set of function binding clauses
     | PatBind      l (GPat id l) (Maybe (GType id l)) (GRhs id l) {-where-} (Maybe (GBinds id l))
     -- ^ A pattern binding
     | ForImp       l (GCallConv l) (Maybe (GSafety l)) (Maybe String) (GName id l) (GType id l)
     -- ^ A foreign import declaration
     | ForExp       l (GCallConv l)                    (Maybe String) (GName id l) (GType id l)
     -- ^ A foreign export declaration
     | RulePragmaDecl   l [GRule id l]
     -- ^ A RULES pragma
     | DeprPragmaDecl   l [([GName id l], String)]
     -- ^ A DEPRECATED pragma
     | WarnPragmaDecl   l [([GName id l], String)]
     -- ^ A WARNING pragma
     | InlineSig        l Bool (Maybe (GActivation l)) (GQName id l)
     -- ^ An INLINE pragma
     | InlineConlikeSig l      (Maybe (GActivation l)) (GQName id l)
     -- ^ An INLINE CONLIKE pragma
     | SpecSig          l      (Maybe (GActivation l)) (GQName id l) [GType id l]
     -- ^ A SPECIALISE pragma
     | SpecInlineSig    l Bool (Maybe (GActivation l)) (GQName id l) [GType id l]
     -- ^ A SPECIALISE INLINE pragma
     | InstSig          l      (Maybe (GContext id l))    (GInstHead id l)
     -- ^ A SPECIALISE instance pragma
     | AnnPragma        l (GAnnotation id l)
     -- ^ An ANN pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An annotation through an ANN pragma.
data GAnnotation id l
    = Ann       l (GName id l)  (GExp id l)
    -- ^ An annotation for a declared name.
    | TypeAnn   l (GName id l)  (GExp id l)
    -- ^ An annotation for a declared type.
    | ModuleAnn l           (GExp id l)
    -- ^ An annotation for the defining module.
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
data GInstHead id l
    = IHead l (GQName id l) [GType id l]
    | IHInfix l (GType id l) (GQName id l) (GType id l)
    | IHParen l (GInstHead id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A deriving clause following a data type declaration.
data GDeriving id l = Deriving l [GInstHead id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A binding group inside a @let@ or @where@ clause.
data GBinds id l
    = BDecls  l [GDecl id l]     -- ^ An ordinary binding group
    | IPBinds l [GIPBind id l]   -- ^ A binding group for implicit parameters
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A binding of an implicit parameter.
data GIPBind id l = IPBind l (GIPName id l) (GExp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Clauses of a function binding.
data GMatch id l
     = Match l      (GName id l) [GPat id l]         (GRhs id l) {-where-} (Maybe (GBinds id l))
        -- ^ A clause defined with prefix notation, i.e. the function name
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
     | InfixMatch l (GPat id l) (GName id l) [GPat id l] (GRhs id l) {-where-} (Maybe (GBinds id l))
        -- ^ A clause defined with infix notation, i.e. first its first argument
        --  pattern, then the function name, then its following argument(s),
        --  the right-hand side and an optional where clause.
        --  Note that there can be more than two arguments to a function declared
        --  infix, hence the list of pattern arguments.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data GQualConDecl id l
    = QualConDecl l
        {-forall-} (Maybe [GTyVarBind id l]) {- . -} (Maybe (GContext id l))
        {- => -} (GConDecl id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declaration of an ordinary data constructor.
data GConDecl id l
     = ConDecl l (GName id l) [GBangType id l]
                -- ^ ordinary data constructor
     | InfixConDecl l (GBangType id l) (GName id l) (GBangType id l)
                -- ^ infix data constructor
     | RecDecl l (GName id l) [GFieldDecl id l]
                -- ^ record constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declaration of a (list of) named field(s).
data GFieldDecl id l = FieldDecl l [GName id l] (GBangType id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A single constructor declaration in a GADT data type declaration.
data GGadtDecl id l
    = GadtDecl l (GName id l) (GType id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declarations inside a class declaration.
data GClassDecl id l
    = ClsDecl    l (GDecl id l)
            -- ^ ordinary declaration
    | ClsDataFam l (Maybe (GContext id l)) (GDeclHead id l) (Maybe (GKind id l))
            -- ^ declaration of an associated data type
    | ClsTyFam   l                     (GDeclHead id l) (Maybe (GKind id l))
            -- ^ declaration of an associated type synonym
    | ClsTyDef   l (GType id l) (GType id l)
            -- ^ default choice for an associated type synonym
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Declarations inside an instance declaration.
data GInstDecl id l
    = InsDecl   l (GDecl id l)
            -- ^ ordinary declaration
    | InsType   l (GType id l) (GType id l)
            -- ^ an associated type definition
    | InsData   l (GDataOrNew l) (GType id l) [GQualConDecl id l] (Maybe (GDeriving id l))
            -- ^ an associated data type implementation
    | InsGData  l (GDataOrNew l) (GType id l) (Maybe (GKind id l)) [GGadtDecl id l] (Maybe (GDeriving id l))
            -- ^ an associated data type implemented using GADT style
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data GBangType id l
     = BangedTy   l (GType id l) -- ^ strict component, marked with \"@!@\"
     | UnBangedTy l (GType id l) -- ^ non-strict component
     | UnpackedTy l (GType id l) -- ^ unboxed component, marked with an UNPACK pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The right hand side of a function or pattern binding.
data GRhs id l
     = UnGuardedRhs l (GExp id l) -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  l [GGuardedRhs id l]
                -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A guarded right hand side @|@ /stmts/ @=@ /exp/.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
data GGuardedRhs id l
     = GuardedRhs l [GStmt id l] (GExp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data GType id l
     = TyForall l
        (Maybe [GTyVarBind id l])
        (Maybe (GContext id l))
        (GType id l)                                -- ^ qualified type
     | TyFun   l (GType id l) (GType id l)              -- ^ function type
     | TyTuple l Boxed [GType id l]                 -- ^ tuple type, possibly boxed
     | TyList  l (GType id l)                       -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyApp   l (GType id l) (GType id l)              -- ^ application of a type constructor
     | TyVar   l (GName id l)                       -- ^ type variable
     | TyCon   l (GQName id l)                      -- ^ named type or type constructor
     | TyParen l (GType id l)                       -- ^ type surrounded by parentheses
     | TyInfix l (GType id l) (GQName id l) (GType id l)    -- ^ infix type constructor
     | TyKind  l (GType id l) (GKind id l)              -- ^ type with explicit kind signature
     -- | TyPromoted l (GPromoted id l)                -- ^ @'K@, a promoted data type (-XDataKinds).
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Bools here are True if there was a leading quote which may be
-- left out. For example @'[k1,k2]@ means the same thing as @[k1,k2]@.
--data Promoted id l
--        = PromotedInteger l Integer String -- ^ parsed value and raw string
--        | PromotedString l String String -- ^ parsed value and raw string
--        | PromotedCon l Bool (GQName id l)
--        | PromotedList l Bool [GPromoted id l]
--        | PromotedTuple l [GPromoted id l]
--        | PromotedUnit l
--  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

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
    -- | KindApp   l (GKind id l) (GKind id l)  -- ^ @k1 k2@
    -- | KindTuple l [GKind id l]           -- ^ @'(k1,k2,k3)@, a promoted tuple
    -- | KindList  l [GKind id l]           -- ^ @'[k1,k2,k3]@, a promoted list literal
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data GFunDep id l
    = FunDep l [GName id l] [GName id l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A context is a set of assertions
data GContext id l
    = CxSingle l (GAsst id l)
    | CxTuple  l [GAsst id l]
    | CxParen  l (GContext id l)
    | CxEmpty  l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data GAsst id l
        = ClassA l (GQName id l) [GType id l]           -- ^ ordinary class assertion
        | InfixA l (GType id l) (GQName id l) (GType id l)  -- ^ class assertion where the class name is given infix
        | IParam l (GIPName id l) (GType id l)          -- ^ implicit parameter assertion
        | EqualP l (GType id l) (GType id l)            -- ^ type equality constraint
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data GLiteral l
    = Char       l Char     String     -- ^ character literal
    | String     l String   String     -- ^ string literal
    | Int        l Integer  String     -- ^ integer literal
    | Frac       l Rational String     -- ^ floating point literal
    | PrimInt    l Integer  String     -- ^ unboxed integer literal
    | PrimWord   l Integer  String     -- ^ unboxed word literal
    | PrimFloat  l Rational String     -- ^ unboxed float literal
    | PrimDouble l Rational String     -- ^ unboxed double literal
    | PrimChar   l Char     String     -- ^ unboxed character literal
    | PrimString l String   String     -- ^ unboxed string literal
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Haskell expressions.
data GExp id l
    = Var l (GQName id l)                       -- ^ variable
    | IPVar l (GIPName id l)                    -- ^ implicit parameter variable
    | Con l (GQName id l)                       -- ^ data constructor
    | Lit l (GLiteral l)                     -- ^ literal constant
    | InfixApp l (GExp id l) (GQOp id l) (GExp id l)    -- ^ infix application
    | App l (GExp id l) (GExp id l)                 -- ^ ordinary application
    | NegApp l (GExp id l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l [GPat id l] (GExp id l)              -- ^ lambda expression
    | Let l (GBinds id l) (GExp id l)               -- ^ local declarations with @let@ ... @in@ ...
    | If l (GExp id l) (GExp id l) (GExp id l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
--    | MultiIf l [GIfAlt id l]                   -- ^ @if@ @|@ /exp/ @->@ /exp/ ...
    | Case l (GExp id l) [GAlt id l]                -- ^ @case@ /exp/ @of@ /alts/
    | Do l [GStmt id l]                         -- ^ @do@-expression:
                                            --   the last statement in the list
                                            --   should be an expression.
    | MDo l [GStmt id l]                        -- ^ @mdo@-expression
    | Tuple l Boxed [GExp id l]                 -- ^ tuple expression
    | TupleSection l Boxed [Maybe (GExp id l)]  -- ^ tuple section expression, e.g. @(,,3)@
    | List l [GExp id l]                        -- ^ list expression
    | Paren l (GExp id l)                       -- ^ parenthesised expression
    | LeftSection l (GExp id l) (GQOp id l)         -- ^ left section @(@/exp/ /qop/@)@
    | RightSection l (GQOp id l) (GExp id l)        -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr l (GQName id l) [GFieldUpdate id l] -- ^ record construction expression
    | RecUpdate l (GExp id l)   [GFieldUpdate id l] -- ^ record update expression
    | EnumFrom l (GExp id l)                    -- ^ unbounded arithmetic sequence,
                                            --   incrementing by 1: @[from ..]@
    | EnumFromTo l (GExp id l) (GExp id l)          -- ^ bounded arithmetic sequence,
                                            --   incrementing by 1 @[from .. to]@
    | EnumFromThen l (GExp id l) (GExp id l)        -- ^ unbounded arithmetic sequence,
                                            --   with first two elements given @[from, then ..]@
    | EnumFromThenTo l (GExp id l) (GExp id l) (GExp id l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given @[from, then .. to]@
    | ListComp l (GExp id l) [GQualStmt id l]       -- ^ ordinary list comprehension
    | ParComp  l (GExp id l) [[GQualStmt id l]]     -- ^ parallel list comprehension
    | ExpTypeSig l (GExp id l) (GType id l)         -- ^ expression with explicit type signature

    | VarQuote l (GQName id l)                  -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote l (GQName id l)                  -- ^ @''T@ for template haskell reifying of types
    | BracketExp l (GBracket id l)              -- ^ template haskell bracket expression
    | SpliceExp l (GSplice id l)                -- ^ template haskell splice expression
    | QuasiQuote l String String            -- ^ quasi-quotaion: @[$/name/| /string/ |]@

-- Hsx
    | XTag l (GXName id l) [GXAttr id l] (Maybe (GExp id l)) [GExp id l]
                                            -- ^ xml element, with attributes and children
    | XETag l (GXName id l) [GXAttr id l] (Maybe (GExp id l))
                                            -- ^ empty xml element, with attributes
    | XPcdata l String                      -- ^ PCDATA child element
    | XExpTag l (GExp id l)                     -- ^ escaped haskell expression inside xml
    | XChildTag l [GExp id l]                   -- ^ children of an xml element


-- Pragmas
    | CorePragma l      String (GExp id l)      -- ^ CORE pragma
    | SCCPragma  l      String (GExp id l)      -- ^ SCC pragma
    | GenPragma  l      String (Int, Int) (Int, Int) (GExp id l)
                                            -- ^ GENERATED pragma

-- Arrows
    | Proc            l (GPat id l) (GExp id l)     -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      l (GExp id l) (GExp id l)     -- ^ arrow application (Gfrom left): /exp/ @-<@ /exp/
    | RightArrApp     l (GExp id l) (GExp id l)     -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  l (GExp id l) (GExp id l)     -- ^ higher-order arrow application (Gfrom left): /exp/ @-<<@ /exp/
    | RightArrHighApp l (GExp id l) (GExp id l)     -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/

-- LambdaCase
    -- | LCase l [GAlt id l]                       -- ^ @\case@ /alts/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The name of an xml element or attribute,
--   possibly qualified with a namespace.
data GXName id l
    = XName l String              -- <name ...
    | XDomName l String String    -- <dom:name ...
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An xml attribute, which is a name-expression pair.
data GXAttr id l = XAttr l (GXName id l) (GExp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A template haskell bracket expression.
data GBracket id l
    = ExpBracket l (GExp id l)        -- ^ expression bracket: @[| ... |]@
    | PatBracket l (GPat id l)        -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket l (GType id l)      -- ^ type bracket: @[t| ... |]@
    | DeclBracket l [GDecl id l]      -- ^ declaration bracket: @[d| ... |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A template haskell splice expression
data GSplice id l
    = IdSplice l String           -- ^ variable splice: @$var@
    | ParenSplice l (GExp id l)       -- ^ parenthesised expression splice: @$(/exp/)@
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
    | AnnModulePragma  l (GAnnotation id l)
                        -- ^ ANN pragma with module scope
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Recognised tools for OPTIONS pragmas.
data Tool = GHC | HUGS | NHC98 | YHC | HADDOCK | UnknownTool String
  deriving (Eq,Ord,Show,Typeable,Data)

-- | Activation clause of a RULES pragma.
data GActivation l
    = ActiveFrom   l Int
    | ActiveUntil  l Int
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The body of a RULES pragma.
data GRule id l
    = Rule l String (Maybe (GActivation l)) (Maybe [GRuleVar id l]) (GExp id l) (GExp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Variables used in a RULES pragma, optionally annotated with types
data GRuleVar id l
    = RuleVar l (GName id l)
    | TypedRuleVar l (GName id l) (GType id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Warning text to optionally use in the module header of e.g.
--   a deprecated module.
data GWarningText l
    = DeprText l String
    | WarnText l String
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | A pattern, to be matched against a value.
data GPat id l
    = PVar l (GName id l)                       -- ^ variable
    | PLit l (GLiteral l)                    -- ^ literal constant
    | PNeg l (GPat id l)                        -- ^ negated pattern
    | PNPlusK l (GName id l) Integer            -- ^ n+k pattern
    | PInfixApp l (GPat id l) (GQName id l) (GPat id l) -- ^ pattern with an infix data constructor
    | PApp l (GQName id l) [GPat id l]              -- ^ data constructor and argument patterns
    | PTuple l Boxed [GPat id l]                -- ^ tuple pattern
    | PList l [GPat id l]                       -- ^ list pattern
    | PParen l (GPat id l)                      -- ^ parenthesized pattern
    | PRec l (GQName id l) [GPatField id l]         -- ^ labelled pattern, record style
    | PAsPat l (GName id l) (GPat id l)             -- ^ @\@@-pattern
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PIrrPat l (GPat id l)                     -- ^ irrefutable pattern: @~/pat/@
    | PatTypeSig l (GPat id l) (GType id l)         -- ^ pattern with type signature
    | PViewPat l (GExp id l) (GPat id l)            -- ^ view patterns of the form @(/exp/ -> /pat/)@
    | PRPat l [GRPat id l]                      -- ^ regular list pattern
    | PXTag l (GXName id l) [GPXAttr id l] (Maybe (GPat id l)) [GPat id l]
                                            -- ^ XML element pattern
    | PXETag l (GXName id l) [GPXAttr id l] (Maybe (GPat id l))
                                            -- ^ XML singleton element pattern
    | PXPcdata l String                     -- ^ XML PCDATA pattern
    | PXPatTag l (GPat id l)                    -- ^ XML embedded pattern
    | PXRPats  l [GRPat id l]                   -- ^ XML regular list pattern
    | PExplTypeArg l (GQName id l) (GType id l)     -- ^ Explicit generics style type argument e.g. @f {| Int |} x = ...@
    | PQuasiQuote l String String           -- ^ quasi quote pattern: @[$/name/| /string/ |]@
    | PBangPat l (GPat id l)                    -- ^ strict (bang) pattern: @f !x = ...@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An XML attribute in a pattern.
data GPXAttr id l = PXAttr l (GXName id l) (GPat id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A regular pattern operator.
data GRPatOp l
    = RPStar  l  -- ^ @*@ = 0 or more
    | RPStarG l  -- ^ @*!@ = 0 or more, greedy
    | RPPlus  l  -- ^ @+@ = 1 or more
    | RPPlusG l  -- ^ @+!@ = 1 or more, greedy
    | RPOpt   l  -- ^ @?@ = 0 or 1
    | RPOptG  l  -- ^ @?!@ = 0 or 1, greedy
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An entity in a regular pattern.
data GRPat id l
    = RPOp l (GRPat id l) (GRPatOp l)   -- ^ operator pattern, e.g. pat*
    | RPEither l (GRPat id l) (GRPat id l) -- ^ choice pattern, e.g. (1 | 2)
    | RPSeq l [GRPat id l]             -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
    | RPGuard l (GPat id l) [GStmt id l]   -- ^ guarded pattern, e.g. (| p | p < 3 |)
    | RPCAs l (GName id l) (GRPat id l)    -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
    | RPAs l (GName id l) (GRPat id l)     -- ^ linear variable binding, e.g. foo\@(1 | 2)
    | RPParen l (GRPat id l)           -- ^ parenthesised pattern, e.g. (2*)
    | RPPat l (GPat id l)              -- ^ an ordinary pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An /fpat/ in a labeled record pattern.
data GPatField id l
    = PFieldPat l (GQName id l) (GPat id l)     -- ^ ordinary label-pattern pair
    | PFieldPun l (GName id l)              -- ^ record field pun
    | PFieldWildcard l                  -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A statement, representing both a /stmt/ in a @do@-expression,
--   an ordinary /qual/ in a list comprehension, as well as a /stmt/
--   in a pattern guard.
data GStmt id l
    = Generator l (GPat id l) (GExp id l)
                            -- ^ a generator: /pat/ @<-@ /exp/
    | Qualifier l (GExp id l)   -- ^ an /exp/ by itself: in a @do@-expression,
                            --   an action whose result is discarded;
                            --   in a list comprehension and pattern guard,
                            --   a guard expression
    | LetStmt l (GBinds id l)   -- ^ local bindings
    | RecStmt l [GStmt id l]    -- ^ a recursive binding group for arrows
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A general /transqual/ in a list comprehension,
--   which could potentially be a transform of the kind
--   enabled by TransformListComp.
data GQualStmt id l
    = QualStmt     l (GStmt id l)         -- ^ an ordinary statement
    | ThenTrans    l (GExp id l)          -- ^ @then@ /exp/
    | ThenBy       l (GExp id l) (GExp id l)  -- ^ @then@ /exp/ @by@ /exp/
    | GroupBy      l (GExp id l)          -- ^ @then@ @group@ @by@ /exp/
    | GroupUsing   l (GExp id l)          -- ^ @then@ @group@ @using@ /exp/
    | GroupByUsing l (GExp id l) (GExp id l)  -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An /fbind/ in a labeled construction or update expression.
data GFieldUpdate id l
    = FieldUpdate l (GQName id l) (GExp id l)    -- ^ ordinary label-expresion pair
    | FieldPun l (GName id l)                -- ^ record field pun
    | FieldWildcard l                    -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An /alt/ alternative in a @case@ expression.
data GAlt id l
    = Alt l (GPat id l) (GGuardedAlts id l) (Maybe (GBinds id l))
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | The right-hand sides of a @case@ alternative,
--   which may be a single right-hand side or a
--   set of guarded ones.
data GGuardedAlts id l
    = UnGuardedAlt l (GExp id l)         -- ^ @->@ /exp/
    | GuardedAlts  l [GGuardedAlt id l]  -- ^ /gdpat/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A guarded case alternative @|@ /stmts/ @->@ /exp/.
data GGuardedAlt id l
    = GuardedAlt l [GStmt id l] (GExp id l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

---- | An alternative in a multiway @if@ expression.
--data IfAlt id l
--    = IfAlt l (GExp id l) (GExp id l)
--  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

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

unit_con :: l -> GExp id l
unit_con l = Con l $ unit_con_name l

tuple_con :: l -> Boxed -> Int -> GExp id l
tuple_con l b i = Con l (tuple_con_name l b i)

unboxed_singleton_con :: l -> GExp id l
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

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: l -> GType id l
unit_tycon l = TyCon l $ unit_tycon_name l
fun_tycon  l = TyCon l $ fun_tycon_name  l
list_tycon l = TyCon l $ list_tycon_name l
unboxed_singleton_tycon l = TyCon l $ unboxed_singleton_tycon_name l

tuple_tycon :: l -> Boxed -> Int -> GType id l
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
    amap f (ModuleName l n) = ModuleName (f l) n

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
        Qual    l mn n  -> l
        UnQual  l    n  -> l
        Special l sc    -> l
    amap f qn = case qn of
        Qual    l mn n  -> Qual    (f l) mn n
        UnQual  l    n  -> UnQual  (f l)    n
        Special l sc    -> Special (f l) sc

instance Annotated (GName id) where
    ann (Ident  l s) = l
    ann (Symbol l s) = l
    amap = fmap

instance Annotated (GIPName id) where
    ann (IPDup l s) = l
    ann (IPLin l s) = l
    amap = fmap

instance Annotated (GQOp id) where
    ann (QVarOp l qn) = l
    ann (QConOp l qn) = l
    amap f (QVarOp l qn) = QVarOp (f l) qn
    amap f (QConOp l qn) = QConOp (f l) qn

instance Annotated (GOp id) where
    ann (VarOp l n) = l
    ann (ConOp l n) = l
    amap f (VarOp l n) = VarOp (f l) n
    amap f (ConOp l n) = ConOp (f l) n

instance Annotated (GCName id) where
    ann (VarName l n) = l
    ann (ConName l n) = l
    amap f (VarName l n) = VarName (f l) n
    amap f (ConName l n) = ConName (f l) n

instance Annotated (GModule id) where
    ann (Module l mmh ops iss dcls) = l
    ann (XmlPage l mn os xn xas me es) = l
    ann (XmlHybrid l mmh ops iss dcls xn xas me es) = l

    amap f (Module l mmh ops iss dcls) =
        Module (f l) mmh ops iss dcls
    amap f (XmlPage l mn os xn xas me es) =
        XmlPage (f l) mn os xn xas me es
    amap f (XmlHybrid l mmh ops iss dcls xn xas me es) =
        XmlHybrid (f l) mmh ops iss dcls xn xas me es

instance Annotated (GModuleHead id) where
    ann (ModuleHead l n mwt mesl) = l
    amap f (ModuleHead l n mwt mesl) = ModuleHead (f l) n mwt mesl

instance Annotated (GExportSpecList id) where
    ann (ExportSpecList l ess) = l
    amap f (ExportSpecList l ess) = ExportSpecList (f l) ess

instance Annotated (GExportSpec id) where
    ann es = case es of
        EVar l qn       -> l
        EAbs l qn       -> l
        EThingAll l qn  -> l
        EThingWith l qn cns -> l
        EModuleContents l mn    -> l
    amap f es = case es of
        EVar l qn       -> EVar (f l) qn
        EAbs l qn       -> EAbs (f l) qn
        EThingAll l qn  -> EThingAll (f l) qn
        EThingWith l qn cns -> EThingWith (f l) qn cns
        EModuleContents l mn    -> EModuleContents (f l) mn

instance Annotated (GImportDecl id) where
    ann (ImportDecl l mn qual src pkg mmn mis) = l
    amap f (ImportDecl l mn qual src pkg mmn mis) =
        ImportDecl (f l) mn qual src pkg mmn mis

instance Annotated (GImportSpecList id) where
    ann (ImportSpecList l b iss) = l
    amap f (ImportSpecList l b iss) = ImportSpecList (f l) b iss

instance Annotated (GImportSpec id) where
    ann is = case is of
        IVar l n        -> l
        IAbs l n        -> l
        IThingAll l n   -> l
        IThingWith l n cns  -> l
        -- IType l m       -> l
    amap f is = case is of
        IVar l n        -> IVar (f l) n
        IAbs l n        -> IAbs (f l) n
        IThingAll l n   -> IThingAll (f l) n
        IThingWith l n cns  -> IThingWith (f l) n cns
        -- IType  l m      -> IType (f l) m

instance Annotated GAssoc where
    ann (AssocNone  l) = l
    ann (AssocLeft  l) = l
    ann (AssocRight l) = l
    amap = fmap

instance Annotated (GDeriving id) where
    ann (Deriving l ihs)    = l
    amap f (Deriving l ihs) = Deriving (f l) ihs

instance Annotated (GDecl id) where
    ann decl = case decl of
        TypeDecl     l dh t         -> l
        TypeFamDecl  l dh mk        -> l
        DataDecl     l dn cx dh cds ders -> l
        GDataDecl    l dn cx dh mk gds ders -> l
        DataFamDecl  l    cx dh mk  -> l
        TypeInsDecl  l t1 t2        -> l
        DataInsDecl  l dn t cds ders    -> l
        GDataInsDecl l dn t mk gds ders -> l
        ClassDecl    l cx dh fds cds    -> l
        InstDecl     l cx ih ids        -> l
        DerivDecl    l cx ih            -> l
        InfixDecl    l a k ops          -> l
        DefaultDecl  l ts               -> l
        SpliceDecl   l sp               -> l
        TypeSig      l ns t             -> l
        FunBind      l ms               -> l
        PatBind      l p mt rhs bs      -> l
        ForImp       l cc msf s n t     -> l
        ForExp       l cc     s n t     -> l
        RulePragmaDecl   l rs           -> l
        DeprPragmaDecl   l nss          -> l
        WarnPragmaDecl   l nss          -> l
        InlineSig        l b act qn     -> l
        InlineConlikeSig l   act qn     -> l
        SpecSig          l   act qn ts  -> l
        SpecInlineSig    l b act qn ts  -> l
        InstSig          l cx ih        -> l
        AnnPragma        l ann          -> l
    amap f decl = case decl of
        TypeDecl     l dh t      -> TypeDecl    (f l) dh t
        TypeFamDecl  l dh mk     -> TypeFamDecl (f l) dh mk
        DataDecl     l dn mcx dh cds ders ->
            DataDecl (f l) dn mcx dh cds ders
        GDataDecl    l dn mcx dh mk gds ders ->
            GDataDecl (f l) dn mcx dh mk gds ders
        DataFamDecl  l mcx dh mk         -> DataFamDecl (f l) mcx dh mk
        TypeInsDecl  l t1 t2             -> TypeInsDecl (f l) t1 t2
        DataInsDecl  l dn t cds ders     -> DataInsDecl (f l) dn t cds ders
        GDataInsDecl l dn t mk gds ders  -> GDataInsDecl (f l) dn t mk gds ders
        ClassDecl    l mcx dh fds cds    -> ClassDecl (f l) mcx dh fds cds
        InstDecl     l mcx ih ids        -> InstDecl (f l) mcx ih ids
        DerivDecl    l mcx ih            -> DerivDecl (f l) mcx ih
        InfixDecl    l a k ops           -> InfixDecl (f l) a k ops
        DefaultDecl  l ts                -> DefaultDecl (f l) ts
        SpliceDecl   l sp                -> SpliceDecl (f l) sp
        TypeSig      l ns t              -> TypeSig (f l) ns t
        FunBind      l ms                -> FunBind (f l) ms
        PatBind      l p mt rhs bs       -> PatBind (f l) p mt rhs bs
        ForImp       l cc msf s n t      -> ForImp (f l) cc msf s n t
        ForExp       l cc     s n t      -> ForExp (f l) cc     s n t
        RulePragmaDecl   l rs            -> RulePragmaDecl (f l) rs
        DeprPragmaDecl   l nss           -> DeprPragmaDecl (f l) nss
        WarnPragmaDecl   l nss           -> WarnPragmaDecl (f l) nss
        InlineSig        l b act qn      -> InlineSig (f l) b act qn
        InlineConlikeSig l   act qn      -> InlineConlikeSig (f l) act qn
        SpecSig          l   act qn ts   -> SpecSig       (f l)   act qn ts
        SpecInlineSig    l b act qn ts   -> SpecInlineSig (f l) b act qn ts
        InstSig          l mcx ih        -> InstSig (f l) mcx ih
        AnnPragma        l ann           -> AnnPragma (f l) ann

instance Annotated (GAnnotation id) where
    ann (Ann     l n e) = l
    ann (TypeAnn l n e) = l
    ann (ModuleAnn l e) = l
    amap f (Ann     l n e) = Ann     (f l) n e
    amap f (TypeAnn l n e) = TypeAnn (f l) n e
    amap f (ModuleAnn l e) = ModuleAnn (f l) e

instance Annotated GDataOrNew where
    ann (DataType l) = l
    ann (NewType  l) = l
    amap = fmap

instance Annotated (GDeclHead id) where
    ann (DHead l n tvs)       = l
    ann (DHInfix l tva n tvb) = l
    ann (DHParen l dh)        = l
    amap f (DHead l n tvs)       = DHead (f l) n tvs
    amap f (DHInfix l tva n tvb) = DHInfix (f l) tva n tvb
    amap f (DHParen l dh)        = DHParen (f l) dh

instance Annotated (GInstHead id) where
    ann (IHead l qn ts) = l
    ann (IHInfix l ta qn tb) = l
    ann (IHParen l ih) = l
    amap f (IHead l qn ts)       = IHead (f l) qn ts
    amap f (IHInfix l ta qn tb)  = IHInfix (f l) ta qn tb
    amap f (IHParen l ih)        = IHParen (f l) ih

instance Annotated (GBinds id) where
    ann (BDecls  l decls) = l
    ann (IPBinds l ibs)   = l
    amap f (BDecls  l decls) = BDecls (f l) decls
    amap f (IPBinds l ibs)   = IPBinds (f l) ibs

instance Annotated (GIPBind id) where
    ann (IPBind l ipn e) = l
    amap f (IPBind l ipn e) = IPBind (f l) ipn e

instance Annotated (GMatch id) where
    ann (Match l n ps rhs bs) = l
    ann (InfixMatch l a n b rhs bs) = l
    amap f (Match l n ps rhs bs) = Match (f l) n ps rhs bs
    amap f (InfixMatch l a n b rhs bs) = InfixMatch (f l) a n b rhs bs

instance Annotated (GQualConDecl id) where
    ann (QualConDecl l tvs cx cd) = l
    amap f (QualConDecl l tvs cx cd) = QualConDecl (f l) tvs cx cd

instance Annotated (GConDecl id) where
    ann (ConDecl l n bts) = l
    ann (InfixConDecl l ta n tb) = l
    ann (RecDecl l n nsbts) = l
    amap f (ConDecl l n bts) = ConDecl (f l) n bts
    amap f (InfixConDecl l ta n tb) = InfixConDecl (f l) ta n tb
    amap f (RecDecl l n fds) = RecDecl (f l) n fds

instance Annotated (GFieldDecl id) where
    ann (FieldDecl l ns t) = l
    amap f (FieldDecl l ns t) = FieldDecl (f l) ns t

instance Annotated (GGadtDecl id) where
    ann (GadtDecl l n t) = l
    amap f (GadtDecl l n t) = GadtDecl (f l) n t

instance Annotated (GClassDecl id) where
    ann (ClsDecl    l d) = l
    ann (ClsDataFam l cx dh mk) = l
    ann (ClsTyFam   l    dh mk) = l
    ann (ClsTyDef   l t1 t2) = l
    amap f (ClsDecl    l d) = ClsDecl (f l) d
    amap f (ClsDataFam l mcx dh mk) = ClsDataFam (f l) mcx dh mk
    amap f (ClsTyFam   l     dh mk) = ClsTyFam   (f l)     dh mk
    amap f (ClsTyDef   l t1 t2) = ClsTyDef (f l) t1 t2

instance Annotated (GInstDecl id) where
    ann id = case id of
        InsDecl   l d           -> l
        InsType   l t1 t2       -> l
        InsData   l dn t    cds ders            -> l
        InsGData  l dn t mk gds ders            -> l
--        InsInline l b act qn    -> l
    amap f id = case id of
        InsDecl   l d           -> InsDecl (f l) d
        InsType   l t1 t2       -> InsType (f l) t1 t2
        InsData   l dn t    cds ders -> InsData  (f l) dn t    cds ders
        InsGData  l dn t mk gds ders -> InsGData (f l) dn t mk gds ders
--        InsInline l b act qn    -> InsInline (f l) b act qn

instance Annotated (GBangType id) where
     ann (BangedTy   l t) = l
     ann (UnBangedTy l t) = l
     ann (UnpackedTy l t) = l
     amap f (BangedTy   l t) = BangedTy (f l)   t
     amap f (UnBangedTy l t) = UnBangedTy (f l) t
     amap f (UnpackedTy l t) = UnpackedTy (f l) t

instance Annotated (GRhs id) where
     ann (UnGuardedRhs l e) = l
     ann (GuardedRhss  l grhss) = l
     amap f (UnGuardedRhs l e)     = UnGuardedRhs (f l) e
     amap f (GuardedRhss  l grhss) = GuardedRhss  (f l) grhss

instance Annotated (GGuardedRhs id) where
     ann (GuardedRhs l ss e) = l
     amap f (GuardedRhs l ss e) = GuardedRhs (f l) ss e

instance Annotated (GType id) where
    ann t = case t of
      TyForall l mtvs cx t          -> l
      TyFun   l t1 t2               -> l
      TyTuple l b ts                -> l
      TyList  l t                   -> l
      TyApp   l t1 t2               -> l
      TyVar   l n                   -> l
      TyCon   l qn                  -> l
      TyParen l t                   -> l
      TyInfix l ta qn tb            -> l
      TyKind  l t k                 -> l
--      TyPromoted l   p              -> l
    amap f t = case t of
      TyForall l mtvs mcx t         -> TyForall (f l) mtvs mcx t
      TyFun   l t1 t2               -> TyFun (f l) t1 t2
      TyTuple l b ts                -> TyTuple (f l) b ts
      TyList  l t                   -> TyList (f l) t
      TyApp   l t1 t2               -> TyApp (f l) t1 t2
      TyVar   l n                   -> TyVar (f l) n
      TyCon   l qn                  -> TyCon (f l) qn
      TyParen l t                   -> TyParen (f l) t
      TyInfix l ta qn tb            -> TyInfix (f l) ta qn tb
      TyKind  l t k                 -> TyKind (f l) t k
--      TyPromoted l   p              -> TyPromoted (f l)   p

instance Annotated (GTyVarBind id) where
    ann (KindedVar   l n k) = l
    ann (UnkindedVar l n)   = l
    amap f (KindedVar   l n k) = KindedVar   (f l) n k
    amap f (UnkindedVar l n)   = UnkindedVar (f l) n

instance Annotated (GKind id) where
    ann (KindStar l) = l
    ann (KindBang l) = l
    ann (KindFn   l k1 k2) = l
    ann (KindParen l k) = l
    ann (KindVar l v) = l
    -- ann (KindApp l k1 k2) = l
    -- ann (KindTuple l ks) = l
    -- ann (KindList  l ks) = l
    amap f (KindStar l) = KindStar (f l)
    amap f (KindBang l) = KindBang (f l)
    amap f (KindFn   l k1 k2) = KindFn (f l) k1 k2
    amap f (KindParen l k) = KindParen (f l) k
    amap f (KindVar l n) = KindVar (f l) n
    -- amap f (KindApp l k1 k2) = KindApp (f l) k1 k2
    -- amap f (KindTuple l ks) = KindTuple (f l) ks
    -- amap f (KindList  l ks) = KindList  (f l) ks

instance Annotated (GFunDep id) where
    ann (FunDep l ns1 ns2) = l
    amap f (FunDep l ns1 ns2) = FunDep (f l) ns1 ns2

instance Annotated (GContext id) where
    ann (CxSingle l asst ) = l
    ann (CxTuple  l assts) = l
    ann (CxParen  l ctxt )  = l
    ann (CxEmpty  l)       = l
    amap f (CxSingle l asst ) = CxSingle (f l) asst
    amap f (CxTuple  l assts) = CxTuple  (f l) assts
    amap f (CxParen  l ctxt ) = CxParen  (f l) ctxt
    amap f (CxEmpty l) = CxEmpty (f l)

instance Annotated (GAsst id) where
    ann asst = case asst of
        ClassA l qn ts      -> l
        InfixA l ta qn tb   -> l
        IParam l ipn t      -> l
        EqualP l t1 t2      -> l
    amap f asst = case asst of
        ClassA l qn ts      -> ClassA (f l) qn ts
        InfixA l ta qn tb   -> InfixA (f l) ta qn tb
        IParam l ipn t      -> IParam (f l) ipn t
        EqualP l t1 t2      -> EqualP (f l) t1 t2

instance Annotated GLiteral where
    ann lit = case lit of
        Char    l c    rw  -> l
        String  l s    rw  -> l
        Int     l i    rw  -> l
        Frac    l r    rw  -> l
        PrimInt    l i rw  -> l
        PrimWord   l i rw  -> l
        PrimFloat  l r rw  -> l
        PrimDouble l r rw  -> l
        PrimChar   l c rw  -> l
        PrimString l s rw  -> l
    amap = fmap

instance Annotated (GExp id) where
    ann e = case e of
        Var l qn        -> l
        IPVar l ipn     -> l
        Con l qn        -> l
        Lit l lit       -> l
        InfixApp l e1 qop e2    -> l
        App l e1 e2     -> l
        NegApp l e      -> l
        Lambda l ps e   -> l
        Let l bs e      -> l
        If l ec et ee   -> l
--        MultiIf l alts  -> l
        Case l e alts   -> l
        Do l ss         -> l
        MDo l ss        -> l
        Tuple l bx es   -> l
        TupleSection l bx mes -> l
        List l es       -> l
        Paren l e       -> l
        LeftSection l e qop     -> l
        RightSection l qop e    -> l
        RecConstr l qn fups     -> l
        RecUpdate l e  fups     -> l
        EnumFrom l e            -> l
        EnumFromTo l ef et      -> l
        EnumFromThen l ef et    -> l
        EnumFromThenTo l ef eth eto -> l
        ListComp l e qss        -> l
        ParComp  l e qsss       -> l
        ExpTypeSig l e t        -> l
        VarQuote l qn           -> l
        TypQuote l qn           -> l
        BracketExp l br         -> l
        SpliceExp l sp          -> l
        QuasiQuote l sn se      -> l

        XTag  l xn xas me es     -> l
        XETag l xn xas me        -> l
        XPcdata l s              -> l
        XExpTag l e              -> l
        XChildTag l es           -> l

        CorePragma l s e   -> l
        SCCPragma  l s e   -> l
        GenPragma  l s n12 n34 e -> l

        Proc            l p e   -> l
        LeftArrApp      l e1 e2 -> l
        RightArrApp     l e1 e2 -> l
        LeftArrHighApp  l e1 e2 -> l
        RightArrHighApp l e1 e2 -> l

        -- LCase l alts -> l

    amap f e = case e of
        Var l qn        -> Var (f l) qn
        IPVar l ipn     -> IPVar (f l) ipn
        Con l qn        -> Con (f l) qn
        Lit l lit       -> Lit (f l) lit
        InfixApp l e1 qop e2    -> InfixApp (f l) e1 qop e2
        App l e1 e2     -> App (f l) e1 e2
        NegApp l e      -> NegApp (f l) e
        Lambda l ps e   -> Lambda (f l) ps e
        Let l bs e      -> Let (f l) bs e
        If l ec et ee   -> If (f l) ec et ee
        Case l e alts   -> Case (f l) e alts
        Do l ss         -> Do (f l) ss
        MDo l ss        -> MDo (f l) ss
        Tuple l bx es   -> Tuple (f l) bx es
        TupleSection l bx mes -> TupleSection (f l) bx mes
        List l es       -> List (f l) es
        Paren l e       -> Paren (f l) e
        LeftSection l e qop     -> LeftSection (f l) e qop
        RightSection l qop e    -> RightSection (f l) qop e
        RecConstr l qn fups     -> RecConstr (f l) qn fups
        RecUpdate l e  fups     -> RecUpdate (f l) e  fups
        EnumFrom l e            -> EnumFrom (f l) e
        EnumFromTo l ef et      -> EnumFromTo (f l) ef et
        EnumFromThen l ef et    -> EnumFromThen (f l) ef et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo (f l) ef eth eto
        ListComp l e qss        -> ListComp (f l) e qss
        ParComp  l e qsss       -> ParComp  (f l) e qsss
        ExpTypeSig l e t        -> ExpTypeSig (f l) e t
        VarQuote l qn           -> VarQuote (f l) qn
        TypQuote l qn           -> TypQuote (f l) qn
        BracketExp l br         -> BracketExp (f l) br
        SpliceExp l sp          -> SpliceExp (f l) sp
        QuasiQuote l sn se      -> QuasiQuote (f l) sn se

        XTag  l xn xas me es     -> XTag  (f l) xn xas me es
        XETag l xn xas me        -> XETag (f l) xn xas me
        XPcdata l s              -> XPcdata (f l) s
        XExpTag l e              -> XExpTag (f l) e
        XChildTag l es           -> XChildTag (f l) es

        CorePragma l s e   -> CorePragma (f l) s e
        SCCPragma  l s e   -> SCCPragma (f l) s e
        GenPragma  l s n12 n34 e -> GenPragma (f l) s n12 n34 e

        Proc            l p e   -> Proc (f l) p e
        LeftArrApp      l e1 e2 -> LeftArrApp      (f l) e1 e2
        RightArrApp     l e1 e2 -> RightArrApp     (f l) e1 e2
        LeftArrHighApp  l e1 e2 -> LeftArrHighApp  (f l) e1 e2
        RightArrHighApp l e1 e2 -> RightArrHighApp (f l) e1 e2

        -- LCase l alts -> LCase (f l) alts


instance Annotated (GXName id) where
    ann (XName l s)  = l
    ann (XDomName l sd sn) = l
    amap = fmap

instance Annotated (GXAttr id) where
    ann (XAttr l xn e) = l
    amap f (XAttr l xn e) = XAttr (f l) xn e

instance Annotated (GBracket id) where
    ann (ExpBracket l e) = l
    ann (PatBracket l p) = l
    ann (TypeBracket l t) = l
    ann (DeclBracket l ds) = l
    amap f (ExpBracket l e) = ExpBracket (f l) e
    amap f (PatBracket l p) = PatBracket (f l) p
    amap f (TypeBracket l t) = TypeBracket (f l) t
    amap f (DeclBracket l ds) = DeclBracket (f l) ds

instance Annotated (GSplice id) where
    ann (IdSplice l s) = l
    ann (ParenSplice l e) = l
    amap f (IdSplice l s) = IdSplice (f l) s
    amap f (ParenSplice l e) = ParenSplice (f l) e

instance Annotated GSafety where
    ann (PlayRisky l) = l
    ann (PlaySafe l b) = l
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
    ann (LanguagePragma   l ns) = l
    ann (OptionsPragma    l mt s) = l
    ann (AnnModulePragma  l a) = l
    amap f (LanguagePragma   l ns) = LanguagePragma (f l) ns
    amap f (AnnModulePragma  l a) = AnnModulePragma (f l) a
    amap f p = fmap f p

instance Annotated GActivation where
    ann (ActiveFrom   l k) = l
    ann (ActiveUntil  l k) = l
    amap = fmap

instance Annotated (GRule id) where
    ann (Rule l s act mrvs e1 e2) = l
    amap f (Rule l s act mrvs e1 e2) = Rule (f l) s act mrvs e1 e2

instance Annotated (GRuleVar id) where
    ann (RuleVar l n) = l
    ann (TypedRuleVar l n t) = l
    amap f (RuleVar l n) = RuleVar (f l) n
    amap f (TypedRuleVar l n t) = TypedRuleVar (f l) n t

instance Annotated GWarningText where
    ann (DeprText l s) = l
    ann (WarnText l s) = l
    amap = fmap

instance Annotated (GPat id) where
    ann p = case p of
      PVar l n          -> l
      PLit l lit        -> l
      PNeg l p          -> l
      PNPlusK l n k     -> l
      PInfixApp l pa qn pb  -> l
      PApp l qn ps      -> l
      PTuple l bx ps    -> l
      PList l ps        -> l
      PParen l p        -> l
      PRec l qn pfs     -> l
      PAsPat l n p      -> l
      PWildCard l       -> l
      PIrrPat l p       -> l
      PatTypeSig l p t  -> l
      PViewPat l e p    -> l
      PRPat l rps       -> l
      PXTag l xn pxas mp ps -> l
      PXETag l xn pxas mp   -> l
      PXPcdata l s      -> l
      PXPatTag l p      -> l
      PXRPats  l rps    -> l
      PExplTypeArg l qn t   -> l
      PQuasiQuote l sn st   -> l
      PBangPat l p          -> l
    amap f p = case p of
      PVar l n          -> PVar (f l) n
      PLit l lit        -> PLit (f l) lit
      PNeg l p          -> PNeg (f l) p
      PNPlusK l n k     -> PNPlusK (f l) n k
      PInfixApp l pa qn pb  -> PInfixApp (f l) pa qn pb
      PApp l qn ps      -> PApp (f l) qn ps
      PTuple l bx ps    -> PTuple (f l) bx ps
      PList l ps        -> PList (f l) ps
      PParen l p        -> PParen (f l) p
      PRec l qn pfs     -> PRec (f l) qn pfs
      PAsPat l n p      -> PAsPat (f l) n p
      PWildCard l       -> PWildCard (f l)
      PIrrPat l p       -> PIrrPat (f l) p
      PatTypeSig l p t  -> PatTypeSig (f l) p t
      PViewPat l e p    -> PViewPat (f l) e p
      PRPat l rps       -> PRPat (f l) rps
      PXTag l xn pxas mp ps -> PXTag  (f l) xn pxas mp ps
      PXETag l xn pxas mp   -> PXETag (f l) xn pxas mp
      PXPcdata l s      -> PXPcdata (f l) s
      PXPatTag l p      -> PXPatTag (f l) p
      PXRPats  l rps    -> PXRPats  (f l) rps
      PExplTypeArg l qn t   -> PExplTypeArg (f l) qn t
      PQuasiQuote l sn st   -> PQuasiQuote (f l) sn st
      PBangPat l p          -> PBangPat (f l) p

instance Annotated (GPXAttr id) where
    ann (PXAttr l xn p) = l
    amap f (PXAttr l xn p) = PXAttr (f l) xn p

instance Annotated GRPatOp where
    ann (RPStar  l) = l
    ann (RPStarG l) = l
    ann (RPPlus  l) = l
    ann (RPPlusG l) = l
    ann (RPOpt   l) = l
    ann (RPOptG  l) = l
    amap = fmap

instance Annotated (GRPat id) where
    ann rp = case rp of
      RPOp l rp rop         -> l
      RPEither l rp1 rp2    -> l
      RPSeq l rps           -> l
      RPGuard l p ss        -> l
      RPCAs l n rp          -> l
      RPAs l n rp           -> l
      RPParen l rp          -> l
      RPPat l p             -> l
    amap f rp = case rp of
      RPOp l rp rop         -> RPOp (f l) rp rop
      RPEither l rp1 rp2    -> RPEither (f l) rp1 rp2
      RPSeq l rps           -> RPSeq (f l) rps
      RPGuard l p ss        -> RPGuard (f l) p ss
      RPCAs l n rp          -> RPCAs (f l) n rp
      RPAs l n rp           -> RPAs (f l) n rp
      RPParen l rp          -> RPParen (f l) rp
      RPPat l p             -> RPPat (f l) p

instance Annotated (GPatField id) where
    ann (PFieldPat l qn p) = l
    ann (PFieldPun l n) = l
    ann (PFieldWildcard l) = l
    amap f (PFieldPat l qn p) = PFieldPat (f l) qn p
    amap f (PFieldPun l n) = PFieldPun (f l) n
    amap f (PFieldWildcard l) = PFieldWildcard (f l)

instance Annotated (GStmt id) where
    ann (Generator l p e) = l
    ann (Qualifier l e)   = l
    ann (LetStmt l bs)    = l
    ann (RecStmt l ss)    = l
    amap f (Generator l p e) = Generator (f l) p e
    amap f (Qualifier l e)   = Qualifier (f l) e
    amap f (LetStmt l bs)    = LetStmt (f l) bs
    amap f (RecStmt l ss)    = RecStmt (f l) ss

instance Annotated (GQualStmt id) where
    ann (QualStmt     l s) = l
    ann (ThenTrans    l e) = l
    ann (ThenBy       l e1 e2) = l
    ann (GroupBy      l e) = l
    ann (GroupUsing   l e) = l
    ann (GroupByUsing l e1 e2) = l
    amap f (QualStmt     l s) = QualStmt (f l) s
    amap f (ThenTrans    l e) = ThenTrans (f l) e
    amap f (ThenBy       l e1 e2) = ThenBy (f l) e1 e2
    amap f (GroupBy      l e) = GroupBy (f l) e
    amap f (GroupUsing   l e) = GroupUsing (f l) e
    amap f (GroupByUsing l e1 e2) = GroupByUsing (f l) e1 e2

instance Annotated (GFieldUpdate id) where
    ann (FieldUpdate l qn e) = l
    ann (FieldPun l n)       = l
    ann (FieldWildcard l)    = l
    amap f (FieldUpdate l qn e) = FieldUpdate (f l) qn e
    amap f (FieldPun l n)       = FieldPun (f l) n
    amap f (FieldWildcard l)    = FieldWildcard (f l)

instance Annotated (GAlt id) where
    ann (Alt l p gs bs) = l
    amap f (Alt l p gs bs) = Alt (f l) p gs bs

instance Annotated (GGuardedAlts id) where
    ann (UnGuardedAlt l e) = l
    ann (GuardedAlts  l galts) = l
    amap f (UnGuardedAlt l e) = UnGuardedAlt (f l) e
    amap f (GuardedAlts  l galts) = GuardedAlts (f l) galts

instance Annotated (GGuardedAlt id) where
    ann (GuardedAlt l ss e) = l
    amap f (GuardedAlt l ss e) = GuardedAlt (f l) ss e

--instance Annotated (GPromoted id) where
--    ann (PromotedInteger l int raw) = l
--    ann (PromotedString l str raw) = l
--    ann (PromotedCon l b qn)   = l
--    ann (PromotedList l b ps)  = l
--    ann (PromotedTuple l ps) = l
--    ann (PromotedUnit l)     = l
--    amap f (PromotedInteger l int raw) = PromotedInteger (f l) int raw
--    amap f (PromotedString l str raw) = PromotedString (f l) str raw
--    amap f (PromotedCon l b qn)   = PromotedCon (f l) b qn
--    amap f (PromotedList l b ps)  = PromotedList  (f l) b ps
--    amap f (PromotedTuple l ps) = PromotedTuple (f l) ps
--    amap f (PromotedUnit l)     = PromotedUnit (f l)

--instance Annotated (GIfAlt id) where
--    ann (IfAlt l e1 e2) = l
--    amap f (IfAlt l e1 e2) = IfAlt (f l) e1 e2
