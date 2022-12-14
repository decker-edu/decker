{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Template (expandTemplateMacros) where

import qualified Data.Text as Text
import Relude
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta (lookupMeta)
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Shared
import Text.Pandoc.Walk

expandTemplateMacros :: Pandoc -> Decker Pandoc
expandTemplateMacros (Pandoc meta blocks) =
  -- Walks link blocks first
  return $ Pandoc meta (walk expandLink $ walk expandLinkBlock blocks)
  where
    -- Expands macro links in block contexts
    expandLinkBlock (Para [link@(Link attr text (url, title))]) =
      fromMaybe (Para [link]) (expand attr text url title)
    expandLinkBlock (Plain [link@(Link attr text (url, title))]) =
      fromMaybe (Plain [link]) (expand attr text url title)
    expandLinkBlock block = block

    -- Expands macro links in inline contexts
    expandLink link@(Link attr text (url, title)) =
      fromMaybe link (expand attr text url title)
    expandLink inline = inline

    -- Expand macro and splice back into required context
    expand attr text url title = do
      (name, args) <- parseInvocation text
      let targetArgs = [("url", url), ("title", title)]
      let posArgs = zip (map show [1 .. (length args)]) args
      let allPosArgs = [("args", Text.unwords args)]
      let arguments = allPosArgs <> posArgs <> targetArgs
      template <- lookupMeta ("templates." <> name) meta
      Just $ case template of
        MetaInlines inlines -> splice name $ walk (substituteInline arguments) inlines
        MetaBlocks [Plain blocks] -> splice name $ walk (substituteInline arguments) blocks
        MetaBlocks [Para blocks] -> splice name $ walk (substituteInline arguments) blocks
        MetaBlocks blocks -> splice name $ walk (substituteInline arguments) $ walk (substituteBlock arguments) blocks
        MetaString str -> splice name $ substitute arguments str
        _ -> splice name $ Link attr text (url, title)

    -- Parses a link text into a macro invocation, if possible
    parseInvocation inline =
      case second Text.words $ Text.splitAt 1 $ stringify inline of
        ("@", name : args) -> Just (name, args)
        _ -> Nothing

    -- Substitutes macro arguments into text fragments in various Inline elements
    substituteInline args (Str text) = Str (substitute args text)
    substituteInline args (Link attr text (url, title)) =
      Link attr text (substitute args url, substitute args title)
    substituteInline args (RawInline "html" html) =
      RawInline "html" (substitute args html)
    substituteInline args inline = inline

    -- Substitutes macro arguments into text fragments in RawBlock elements.
    -- Substitution in CodeBlocks is probably not a good idea.
    substituteBlock args (RawBlock "html" html) =
      RawBlock "html" (substitute args html)
    substituteBlock args block = block

    substitute args text = foldl' substituteOne text args

    substituteOne template (name, value) =
      Text.intercalate value $
        Text.splitOn (":(" <> name <> ")") template

class Splice a b where
  splice :: Text -> a -> b

instance Splice Inline Inline where
  splice macro = id

instance Splice Text Inline where
  splice macro = Str

instance Splice Text [Inline] where
  splice macro text = [Str text]

instance Splice Text Block where
  splice macro text = Para [Str text]

instance Splice [Inline] [Inline] where
  splice macro = id

instance Splice [Inline] Inline where
  splice macro = Span nullAttr

instance Splice Block Block where
  splice macro = id

instance Splice [Block] Block where
  splice macro = Div nullAttr

instance Splice Inline Block where
  splice macro block = Para [block]

instance Splice [Inline] Block where
  splice macro = Para

instance Splice Block Inline where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice Block into Inline context"

instance Splice Block [Inline] where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice Block into [Inline] context"

instance Splice [Block] Inline where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice [Block] into Inline context"

instance Splice [Block] [Inline] where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice [Block] into [Inline] context"
