{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.AST.Exts.HSX

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST.Core hiding ( Module, Pat, Exp )

-- | XML extenstions to the @GModule@ type
data Module decl impdecl exp pat id l
    = XmlPage l (ModuleName id l) [ModulePragma id l] (XName id l) [XAttr exp id l] (Maybe exp) [exp]
    -- ^ a module consisting of a single XML document. The ModuleName never appears in the source
    --   but is needed for semantic purposes, it will be the same as the file name.
    | XmlHybrid l (Maybe (ModuleHead id l)) [ModulePragma id l] [impdecl] [decl]
                (XName id l) [XAttr exp id l] (Maybe exp) [exp]
    -- ^ a hybrid module combining an XML document with an ordinary module
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | XML extensions to the @GPat@ type
data Pat pat id l
    = PXTag  l (XName id l) [PXAttr pat id l] (Maybe pat) [pat]
                                            -- ^ XML element pattern
    | PXETag l (XName id l) [PXAttr pat id l] (Maybe pat)
                                            -- ^ XML singleton element pattern
    | PXPcdata l String                     -- ^ XML PCDATA pattern
    | PXPatTag l pat            -- ^ XML embedded pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An XML attribute in a pattern.
data PXAttr pat id l = PXAttr l (XName id l) pat
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | The name of an xml element or attribute,
--   possibly qualified with a namespace.
data XName id l
    = XName l String              -- <name ...
    | XDomName l String String    -- <dom:name ...
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An xml attribute, which is a name-expression pair.
data XAttr exp id l = XAttr l (XName id l) exp
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | XML extensions to the @GExt@ type
data Exp exp lit id l
    = XTag l (XName id l) [XAttr exp id l] (Maybe exp) [exp]
                                            -- ^ xml element, with attributes and children
    | XETag l (XName id l) [XAttr exp id l] (Maybe exp)
                                            -- ^ empty xml element, with attributes
    | XPcdata l String                      -- ^ PCDATA child element
    | XExpTag l exp                     -- ^ escaped haskell expression inside xml
    | XChildTag l [exp]                   -- ^ children of an xml element
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (Module decl impdecl exp pat id) where
    ann (XmlPage l _ _ _ _ _ _) = l
    ann (XmlHybrid l _ _ _ _ _ _ _ _) = l


instance Annotated (Pat pat id) where
    ann p = case p of
      PXTag    l _ _ _ _ -> l
      PXETag   l _ _ _   -> l
      PXPcdata l _       -> l
      PXPatTag l _       -> l


instance Annotated (PXAttr pat id) where
    ann (PXAttr l _ _) = l


instance Annotated (XName id) where
    ann (XName l _)  = l
    ann (XDomName l _ _) = l

instance Annotated (XAttr exp id) where
    ann (XAttr l _ _) = l

instance Annotated (Exp exp lit id) where
    ann e = case e of
      XTag      l _ _ _ _  -> l
      XETag     l _ _ _    -> l
      XPcdata   l _        -> l
      XExpTag   l _        -> l
      XChildTag l _        -> l
