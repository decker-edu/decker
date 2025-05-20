{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | This provides a variety of optics for traversing and
-- destructuring Pandoc documents.
--
-- Note that both 'Inline', 'Block', and 'MetaValue' have 'Plated' instances
-- which are useful for traversing the AST.
--
-- This file was taken out of https://github.com/bgamari/pandoc-lens.git at d7959c9.
-- The original repo does not seem to be maintained very actively and will not compile
-- with lts-13.3.
module Text.Pandoc.Lens
  ( -- * Documents
    Pandoc,
    body,
    meta,

    -- * Blocks

    -- | Prisms are provided for the constructors of 'Block'
    -- as well as a 'Plated' instance.
    Block,
    _Plain,
    _Para,
    _CodeBlock,
    _BlockQuote,
    _OrderedList,
    _BulletList,
    _DefinitionList,
    _Header,
    _HorizontalRule,
    _Div,
    _Null,

    -- * Inlines

    -- | Prisms are provided for the constructors of 'Inline'
    -- as well as a 'Plated' instance.
    Inline,
    _Str,
    _Emph,
    _Strong,
    _Strikeout,
    _Superscript,
    _Subscript,
    _SmallCaps,
    _Quoted,
    _Cite,
    _Code,
    _Space,
    _LineBreak,
    _Math,
    _RawInline,
    _Link,
    _Image,
    _Note,
    _Span,
    inlinePrePlate,

    -- * Metadata

    -- | Prisms are provided for the constructors of 'MetaValue'
    -- as well as a 'Plated' instance.
    MetaValue,
    _MetaMap,
    _MetaList,
    _MetaBool,
    _MetaString,
    _MetaInlines,
    _MetaBlocks,

    -- * Attributes
    HasAttr (..),
    attrIdentifier,
    attrClasses,
    attrs,
  )
where

import Control.Lens
import Data.Map (Map)
import qualified Data.Text as Text
import Text.Pandoc.Definition

-- | The body of a pandoc document
body :: Lens' Pandoc [Block]
body = lens (\(Pandoc _ b) -> b) (\(Pandoc m _) b -> Pandoc m b)

-- | A traversal focusing on a particular metadata value of a document
meta :: Text.Text -> Traversal' Pandoc MetaValue
meta name = metaL . _Wrapped' . ix name
  where
    metaL :: Lens' Pandoc Meta
    metaL = lens (\(Pandoc m _) -> m) (\(Pandoc _ a) m -> Pandoc m a)

instance Wrapped Meta where
  type Unwrapped Meta = Map Text.Text MetaValue
  _Wrapped' = iso unMeta Meta

type instance Index Meta = Text.Text

type instance IxValue Meta = MetaValue

instance Ixed Meta where
  ix k = _Wrapped' . ix k

instance At Meta where
  at k = _Wrapped' . at k

-- | A prism on a 'Plain' 'Block'
_Plain :: Prism' Block [Inline]
_Plain = prism' Plain f
  where
    f (Plain x) = Just x
    f _ = Nothing

-- | A prism on a paragraph 'Block'
_Para :: Prism' Block [Inline]
_Para = prism' Para f
  where
    f (Para x) = Just x
    f _ = Nothing

-- | A prism on the text of a 'CodeBlock'
_CodeBlock :: Prism' Block Text.Text
_CodeBlock = prism' (CodeBlock nullAttr) f
  where
    f (CodeBlock _ x) = Just x
    f _ = Nothing

-- | A prism on a 'BlockQuote'
_BlockQuote :: Prism' Block [Block]
_BlockQuote = prism' BlockQuote f
  where
    f (BlockQuote x) = Just x
    f _ = Nothing

-- | A prism on the items of a bullet list 'Block'
_OrderedList :: Prism' Block (ListAttributes, [[Block]])
_OrderedList = prism' (uncurry OrderedList) f
  where
    f (OrderedList x y) = Just (x, y)
    f _ = Nothing

-- | A prism on the items of a bullet list 'Block'
_BulletList :: Prism' Block [[Block]]
_BulletList = prism' BulletList f
  where
    f (BulletList x) = Just x
    f _ = Nothing

-- | A prism on the items of a definition list 'Block'
_DefinitionList :: Prism' Block [([Inline], [[Block]])]
_DefinitionList = prism' DefinitionList f
  where
    f (DefinitionList x) = Just x
    f _ = Nothing

-- | A prism on a 'Header' 'Block'
_Header :: Prism' Block (Int, [Inline])
_Header = prism' (\(a, b) -> Header a nullAttr b) f
  where
    f (Header a _ b) = Just (a, b)
    f _ = Nothing

-- | A prism on a 'HorizontalRule' 'Block'
_HorizontalRule :: Prism' Block ()
_HorizontalRule = prism' (const HorizontalRule) f
  where
    f HorizontalRule = Just ()
    f _ = Nothing

-- | A prism on a 'Div' 'Block'
_Div :: Prism' Block [Block]
_Div = prism' (Div nullAttr) f
  where
    f (Div _ a) = Just a
    f _ = Nothing

-- | A prism on a 'Null' 'Block'
_Null :: Prism' Block ()
_Null = prism' (const Null) f
  where
    f Null = Just ()
    f _ = Nothing

-- | A prism on a 'Str' 'Inline'
_Str :: Prism' Inline Text.Text
_Str = prism' Str f
  where
    f (Str s) = Just s
    f _ = Nothing

-- | A prism on an 'Emph' 'Inline'
_Emph :: Prism' Inline [Inline]
_Emph = prism' Emph f
  where
    f (Emph s) = Just s
    f _ = Nothing

-- | A prism on a 'Strong' 'Inline'
_Strong :: Prism' Inline [Inline]
_Strong = prism' Strong f
  where
    f (Strong s) = Just s
    f _ = Nothing

-- | A prism on a 'Strikeout' 'Inline'
_Strikeout :: Prism' Inline [Inline]
_Strikeout = prism' Strikeout f
  where
    f (Strikeout s) = Just s
    f _ = Nothing

-- | A prism on a 'Superscript' 'Inline'
_Superscript :: Prism' Inline [Inline]
_Superscript = prism' Superscript f
  where
    f (Superscript s) = Just s
    f _ = Nothing

-- | A prism on a 'Subscript' 'Inline'
_Subscript :: Prism' Inline [Inline]
_Subscript = prism' Subscript f
  where
    f (Subscript s) = Just s
    f _ = Nothing

-- | A prism on a 'SmallCaps' 'Inline'
_SmallCaps :: Prism' Inline [Inline]
_SmallCaps = prism' SmallCaps f
  where
    f (SmallCaps s) = Just s
    f _ = Nothing

-- | A prism on a 'Quoted' 'Inline'
_Quoted :: Prism' Inline (QuoteType, [Inline])
_Quoted = prism' (uncurry Quoted) f
  where
    f (Quoted a b) = Just (a, b)
    f _ = Nothing

-- | A prism on a 'Cite' 'Inline'
_Cite :: Prism' Inline ([Citation], [Inline])
_Cite = prism' (uncurry Cite) f
  where
    f (Cite a b) = Just (a, b)
    f _ = Nothing

-- | A prism on the body of a 'Code' 'Inline'
_Code :: Prism' Inline Text.Text
_Code = prism' (Code nullAttr) f
  where
    f (Code _ s) = Just s
    f _ = Nothing

-- | A prism on a 'Space' 'Inline'
_Space :: Prism' Inline ()
_Space = prism' (const Space) f
  where
    f Space = Just ()
    f _ = Nothing

-- | A prism on a 'LineBreak' 'Inline'
_LineBreak :: Prism' Inline ()
_LineBreak = prism' (const LineBreak) f
  where
    f LineBreak = Just ()
    f _ = Nothing

-- | A prism on a 'Math' 'Inline'
_Math :: Prism' Inline (MathType, Text.Text)
_Math = prism' (uncurry Math) f
  where
    f (Math a b) = Just (a, b)
    f _ = Nothing

-- | A prism on a 'RawInline' 'Inline'
_RawInline :: Prism' Inline (Format, Text.Text)
_RawInline = prism' (uncurry RawInline) f
  where
    f (RawInline a b) = Just (a, b)
    f _ = Nothing

-- | A prism on a 'Link' 'Inline'
_Link :: Prism' Inline ([Inline], Target)
_Link = prism' (uncurry $ Link nullAttr) f
  where
    f (Link _ a b) = Just (a, b)
    f _ = Nothing

-- | A prism on a 'Image' 'Inline'
_Image :: Prism' Inline ([Inline], Target)
_Image = prism' (uncurry $ Image nullAttr) f
  where
    f (Image _ a b) = Just (a, b)
    f _ = Nothing

-- | A prism on a 'Note' 'Inline'
_Note :: Prism' Inline [Block]
_Note = prism' Note f
  where
    f (Note s) = Just s
    f _ = Nothing

-- | A prism on a 'Span' 'Inline'
_Span :: Prism' Inline [Inline]
_Span = prism' (Span nullAttr) f
  where
    f (Span _ s) = Just s
    f _ = Nothing

-- | An affine traversal over the '[Inline]' in the last argument of an 'Inline' constructor
inlinePrePlate :: Traversal' Inline [Inline]
inlinePrePlate f inl =
  case inl of
    Emph cs -> Emph <$> f cs
    Strong cs -> Strong <$> f cs
    Strikeout cs -> Strikeout <$> f cs
    Superscript cs -> Superscript <$> f cs
    Subscript cs -> Subscript <$> f cs
    SmallCaps cs -> SmallCaps <$> f cs
    Quoted q cs -> Quoted q <$> f cs
    Cite cit cs -> Cite cit <$> f cs
    Span attrs cs -> Span attrs <$> f cs
    _ -> pure inl

instance Plated Inline where
  plate = inlinePrePlate . each

-- | A prism on a piece of 'MetaMap' metadata
_MetaMap :: Prism' MetaValue (Map Text.Text MetaValue)
_MetaMap = prism' MetaMap f
  where
    f (MetaMap x) = Just x
    f _ = Nothing

-- | A prism on a piece of 'MetaList' metadata
_MetaList :: Prism' MetaValue [MetaValue]
_MetaList = prism' MetaList f
  where
    f (MetaList x) = Just x
    f _ = Nothing

-- | A prism on a piece of 'MetaBool' metadata
_MetaBool :: Prism' MetaValue Bool
_MetaBool = prism' MetaBool f
  where
    f (MetaBool x) = Just x
    f _ = Nothing

-- | A prism on a piece of 'MetaText.Text' metadata
_MetaString :: Prism' MetaValue Text.Text
_MetaString = prism' MetaString f
  where
    f (MetaString x) = Just x
    f _ = Nothing

-- | A prism on a piece of 'MetaInlines' metadata
_MetaInlines :: Prism' MetaValue [Inline]
_MetaInlines = prism' MetaInlines f
  where
    f (MetaInlines x) = Just x
    f _ = Nothing

-- | A prism on a piece of 'MetaBlocks' metadata
_MetaBlocks :: Prism' MetaValue [Block]
_MetaBlocks = prism' MetaBlocks f
  where
    f (MetaBlocks x) = Just x
    f _ = Nothing

instance Plated MetaValue where
  plate f inl =
    case inl of
      MetaMap cs -> MetaMap <$> traverseOf each f cs
      MetaList cs -> MetaList <$> traverseOf each f cs
      _ -> pure inl

-- | An object that has attributes
class HasAttr a where
  -- | A traversal over the attributes of an object
  attributes :: Traversal' a Attr

instance HasAttr Block where
  attributes f (CodeBlock a s) = fmap (\a' -> CodeBlock a' s) (f a)
  attributes f (Header n a s) = fmap (\a' -> Header n a' s) (f a)
  attributes f (Div a s) = fmap (\a' -> Div a' s) (f a)
  attributes _ x = pure x

instance HasAttr Inline where
  attributes f (Code a s) = fmap (\a' -> Code a' s) (f a)
  attributes f (Span a s) = fmap (\a' -> Span a' s) (f a)
  attributes _ x = pure x

-- | A lens onto identifier of an 'Attr'
attrIdentifier :: Lens' Attr Text.Text
attrIdentifier = _1

-- | A lens onto classes of an 'Attr'
attrClasses :: Lens' Attr [Text.Text]
attrClasses = _2

-- | A lens onto the key-value pairs of an 'Attr'
attrs :: Lens' Attr [(Text.Text, Text.Text)]
attrs = _3
