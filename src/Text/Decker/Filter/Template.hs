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
expandTemplateMacros pandoc@(Pandoc meta _) =
  return $ walk expandLink $ walk expandLinkBlock pandoc
  where
    expandLink link@(Link attr text (url, title)) =
      fromMaybe link (expand attr text url title)
    expandLink inline = inline
    expandLinkBlock (Para [link@(Link attr text (url, title))]) =
      Para [fromMaybe link (expand attr text url title)]
    expandLinkBlock (Plain [link@(Link attr text (url, title))]) =
      Plain [fromMaybe link (expand attr text url title)]
    expandLinkBlock block = block
    expand attr text url title = do
      (name, args) <- parseInvocation text
      template <- lookupMeta ("templates." <> name) meta
      let args' = zip (map show [1 .. (length args)]) args <> [("url", url), ("title", title)]
      return $ substituteParams template args'
    parseInvocation inline =
      case second Text.words $ Text.splitAt 1 $ stringify inline of
        ("@", name : args) -> Just (name, args)
        _ -> Nothing
    substituteParams (MetaInlines template) args =
      Span nullAttr $ walk (substituteInline args) template
    -- substituteParams (MetaBlocks template) args =
    --   Span nullAttr $ (walk (substituteInline args)) template
    substituteParams metaValue args = Str "Cannto parse template"
    substituteInline args (Str text) = Str (substitute args text)
    substituteInline args (Link attr text (url, title)) =
      Link attr text (substitute args url, substitute args title)
    substituteInline args inline = inline
    substitute args text = foldl' substituteOne text args
    substituteOne template (name, value) =
      Text.intercalate value $
        Text.splitOn (":(" <> name <> ")") template
