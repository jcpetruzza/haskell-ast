{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Language.Haskell.Ext.AST.HSX

where

import Data.Data
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.AST hiding ( GModule, GPat, GExp )

-- | XML extenstions to the @GModule@ type
data GModule decl impdecl exp pat id l
    = XmlPage l (GModuleName id l) [GModulePragma id l] (GXName id l) [GXAttr exp id l] (Maybe exp) [exp]
    -- ^ a module consisting of a single XML document. The ModuleName never appears in the source
    --   but is needed for semantic purposes, it will be the same as the file name.
    | XmlHybrid l (Maybe (GModuleHead id l)) [GModulePragma id l] [impdecl] [decl]
                (GXName id l) [GXAttr exp id l] (Maybe exp) [exp]
    -- ^ a hybrid module combining an XML document with an ordinary module
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | XML extensions to the @GPat@ type
data GPat pat id l
    = PXTag  l (GXName id l) [GPXAttr pat id l] (Maybe pat) [pat]
                                            -- ^ XML element pattern
    | PXETag l (GXName id l) [GPXAttr pat id l] (Maybe pat)
                                            -- ^ XML singleton element pattern
    | PXPcdata l String                     -- ^ XML PCDATA pattern
    | PXPatTag l pat            -- ^ XML embedded pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An XML attribute in a pattern.
data GPXAttr pat id l = PXAttr l (GXName id l) pat
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


-- | The name of an xml element or attribute,
--   possibly qualified with a namespace.
data GXName id l
    = XName l String              -- <name ...
    | XDomName l String String    -- <dom:name ...
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | An xml attribute, which is a name-expression pair.
data GXAttr exp id l = XAttr l (GXName id l) exp
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)

-- | XML extensions to the @GExt@ type
data GExp exp lit id l
    = XTag l (GXName id l) [GXAttr exp id l] (Maybe exp) [exp]
                                            -- ^ xml element, with attributes and children
    | XETag l (GXName id l) [GXAttr exp id l] (Maybe exp)
                                            -- ^ empty xml element, with attributes
    | XPcdata l String                      -- ^ PCDATA child element
    | XExpTag l exp                     -- ^ escaped haskell expression inside xml
    | XChildTag l [exp]                   -- ^ children of an xml element
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor)


instance Annotated (GModule decl impdecl exp pat id) where
    ann (XmlPage l _ _ _ _ _ _) = l
    ann (XmlHybrid l _ _ _ _ _ _ _ _) = l

    amap = fmap


instance Annotated (GPat pat id) where
    ann p = case p of
      PXTag    l _ _ _ _ -> l
      PXETag   l _ _ _   -> l
      PXPcdata l _       -> l
      PXPatTag l _       -> l
    amap = fmap


instance Annotated (GPXAttr pat id) where
    ann (PXAttr l _ _) = l
    amap = fmap


instance Annotated (GXName id) where
    ann (XName l _)  = l
    ann (XDomName l _ _) = l
    amap = fmap


instance Annotated (GXAttr exp id) where
    ann (XAttr l _ _) = l
    amap = fmap

instance Annotated (GExp exp lit id) where
    ann e = case e of
      XTag      l _ _ _ _  -> l
      XETag     l _ _ _    -> l
      XPcdata   l _        -> l
      XExpTag   l _        -> l
      XChildTag l _        -> l

    amap = fmap