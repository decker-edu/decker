module Text.Decker.Filter.ShortLink
  ( evaluateShortLinks
  , fillTemplate
  , evalUrl
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

import Data.List
import Data.List.Split
import qualified Data.Text as Text
import Network.URI
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Walk

evaluateShortLinks :: Pandoc -> Decker Pandoc
evaluateShortLinks pandoc@(Pandoc meta _) =
  return $ walk (evalLinks meta) pandoc

evalLinks :: Meta -> Inline -> Inline
evalLinks meta (Link attr alt (url, title)) =
  Link attr alt (evalUrl meta url, title)
evalLinks meta (Image attr alt (url, title)) =
  Image attr alt (evalUrl meta url, title)
evalLinks meta inline = inline

evalUrl :: Meta -> Text.Text -> Text.Text
evalUrl meta url =
  case parseURI (Text.unpack url) of
    Just uri
      | (not . null . uriScheme) uri -> maybe url Text.pack (evalUri meta uri)
    Nothing -> url

evalUri :: Meta -> URI -> Maybe String
evalUri meta uri = do
  let scheme = filter (/= ':') (uriScheme uri)
      path = uriPath uri
  case getMetaString ("short-links" <.> scheme) meta of
    Just template -> fillTemplate template path
    Nothing -> do
      binding <- getMetaString ("short-links" <.> "bind" <.> scheme) meta
      template <- getMetaString ("short-links" <.> scheme <.> binding) meta
      fillTemplate template path

fillTemplate template path =
  case splitOn "@@@" template of
    match
      | length match > 1 ->
        Just $ intercalate (escapeURIString isUnreserved path) match
    _ ->
      case splitOn "@@" template of
        match
          | length match > 1 -> Just $ intercalate path match
        _ -> Just template

a <.> b = a ++ "." ++ b

dot = intercalate "."
