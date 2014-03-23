{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor, FlexibleContexts #-}
module Language.Haskell.AST.Sugar

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding (GExp, GPat)


-- | Extension of @GPat@ with standard syntactic sugar
data GPat lit pat id l
     = PLit l lit                                       -- ^ literal constant
     | PNeg l (pat id l)                                -- ^ negated pattern
     | PInfixApp l (pat id l) (GQName id l) (pat id l)  -- ^ pattern with an infix data constructor
     | PTuple l Boxed [pat id l]                        -- ^ tuple pattern
     | PList l [pat id l]                               -- ^ list pattern
     | PParen l (pat id l)                              -- ^ parenthesized pattern
     | PRec l (GQName id l) [GPatField pat id l]        -- ^ labelled pattern, record style
     | PAsPat l (GName id l) (pat id l)                 -- ^ @\@@-pattern
     | PIrrPat l (pat id l)                             -- ^ irrefutable pattern: @~/pat/@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An /fpat/ in a labeled record pattern.
data GPatField pat id l
    = PFieldPat l (GQName id l) (pat id l)    -- ^ ordinary label-pattern pair
    | PFieldPun l (GName id l)              -- ^ record field pun
    | PFieldWildcard l                  -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | Extension of @GExp@ with standard syntactic sugar
-- (qstmt are the statements allowed inside a list comprehension; for ordinary comprehensions, use Stmt)
data GExp ty qstmt stmt exp id l
     = InfixApp l (exp id l) (GQOp id l) (exp id l)  -- ^ infix application
     | NegApp l (exp id l)                           -- ^ negation expression @-/exp/@ (unary minus)
     | If l (exp id l) (exp id l) (exp id l)         -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
     | Do l [stmt id l]                              -- ^ @do@-expression:
                                                     --   the last statement in the list
                                                     --   should be an expression.
     | Tuple l Boxed [exp id l]                      -- ^ tuple expression
     | TupleSection l Boxed [Maybe (exp id l)]       -- ^ tuple section expression, e.g. @(,,3)@
     | List l [exp id l]                             -- ^ list expression
     | Paren l (exp id l)                            -- ^ parenthesised expression
     | LeftSection l (exp id l) (GQOp id l)          -- ^ left section @(@/exp/ /qop/@)@
     | RightSection l (GQOp id l) (exp id l)         -- ^ right section @(@/qop/ /exp/@)@
     | RecConstr l (GQName id l) [GFieldUpdate exp id l] -- ^ record construction expression
     | RecUpdate l (exp id l)  [GFieldUpdate exp id l]   -- ^ record update expression
     | EnumFrom l (exp id l)                         -- ^ unbounded arithmetic sequence,
                                                     --   incrementing by 1: @[from ..]@
     | EnumFromTo l (exp id l) (exp id l)            -- ^ bounded arithmetic sequence,
                                                     --   incrementing by 1 @[from .. to]@
     | EnumFromThen l (exp id l) (exp id l)          -- ^ unbounded arithmetic sequence,
                                                     --   with first two elements given @[from, then ..]@
     | EnumFromThenTo l (exp id l) (exp id l) (exp id l)
                                                     -- ^ bounded arithmetic sequence,
                                                     --   with first two elements given @[from, then .. to]@
     | ListComp l (exp id l) [qstmt]                 -- ^ ordinary list comprehension
     | ExpTypeSig l (exp id l) ty                    -- ^ expression with explicit type signature
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data GQOp id l
    = QVarOp l (GQName id l) -- ^ variable operator (/qvarop/)
    | QConOp l (GQName id l) -- ^ constructor operator (/qconop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An /fbind/ in a labeled construction or update expression.
data GFieldUpdate exp id l
    = FieldUpdate l (GQName id l) (exp id l) -- ^ ordinary label-expresion pair
    | FieldPun l (GName id l)                -- ^ record field pun
    | FieldWildcard l                        -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | A statement, representing both a /stmt/ in a @do@-expression,
--   an ordinary /qual/ in a list comprehension, as well as a /stmt/
--   in a pattern guard.
data GStmt binds exp pat stmtext id l
    = Generator l (pat id l) (exp id l)  -- ^ a generator: /pat/ @<-@ /exp/
    | Qualifier l (exp id l)             --   an action whose result is discarded;
                                         --   in a list comprehension and pattern guard,
                                         --   a guard expression
    | LetStmt l binds                    -- ^ local bindings
    | StmtExt (stmtext id l)             -- ^ an extended statement
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Functor (pat id) => Annotated (GPat lit pat id) where
    ann p = case p of
      PLit l _          -> l
      PNeg l _          -> l
      PInfixApp l _ _ _ -> l
      PTuple l _ _      -> l
      PList l _         -> l
      PParen l _        -> l
      PRec l _ _        -> l
      PAsPat l _ _      -> l
      PIrrPat l _       -> l
    amap = fmap

instance Functor (pat id) => Annotated (GPatField pat id) where
    ann (PFieldPat l _ _)  = l
    ann (PFieldPun l _)    = l
    ann (PFieldWildcard l) = l
    amap = fmap


instance (Functor (stmt id), Functor (exp id))
  => Annotated (GExp ty qstmt stmt exp id) where
    ann e = case e of
        InfixApp l _ _ _       -> l
        NegApp l _             -> l
        If l _ _ _             -> l
        Do l _                 -> l
        Tuple l _ _            -> l
        TupleSection l _ _     -> l
        List l _               -> l
        Paren l _              -> l
        LeftSection l _ _      -> l
        RightSection l _ _     -> l
        RecConstr l _ _        -> l
        RecUpdate l _  _       -> l
        EnumFrom l _           -> l
        EnumFromTo l _ _       -> l
        EnumFromThen l _ _     -> l
        EnumFromThenTo l _ _ _ -> l
        ListComp l _ _         -> l
        ExpTypeSig l _ _       -> l
    amap = fmap

instance Annotated (GQOp id) where
    ann (QVarOp l _) = l
    ann (QConOp l _) = l
    amap = fmap

instance Functor (exp id) => Annotated (GFieldUpdate exp id) where
    ann (FieldUpdate l _ _) = l
    ann (FieldPun l _)      = l
    ann (FieldWildcard l)   = l
    amap = fmap

instance (Functor (exp id), Functor (pat id), Annotated (stmtext id))
 => Annotated (GStmt binds exp pat stmtext id) where
    ann (Generator l _ _) = l
    ann (Qualifier l _)   = l
    ann (LetStmt l _)     = l
    ann (StmtExt e)       = ann e
    amap = fmap

