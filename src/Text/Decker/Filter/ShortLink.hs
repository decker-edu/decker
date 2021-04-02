{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.ShortLink
  ( evaluateShortLinks,
    fillTemplate,
    evalUrl,
  )
where

import Data.List
import Data.List.Split
import Network.URI
import Relude
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI (makeProjectPath)
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Walk

evaluateShortLinks :: Pandoc -> Decker Pandoc
evaluateShortLinks pandoc@(Pandoc meta _) = do
  base <- gets basePath
  return $ walk (evalLinks meta base) pandoc
  where
    evalLinks :: Meta -> String -> Inline -> Inline
    evalLinks meta base (Link attr alt (url, title)) =
      Link attr alt (evalUrl meta url, title)
    evalLinks meta base (Image attr alt (url, title)) =
      let u = toText $ makeProjectPath base (toString url)
       in Image attr alt (evalUrl meta u, title)
    evalLinks meta base inline = inline

evalUrl :: Meta -> Text -> Text
evalUrl meta url =
  case parseURI (toString url) of
    Just uri
      | (not . null . uriScheme) uri -> maybe url toText (evalUri meta uri)
    _ -> url

evalUri :: Meta -> URI -> Maybe String
evalUri meta uri = do
  let scheme = toText $ filter (/= ':') (uriScheme uri)
      path = uriPath uri
  case lookupMeta ("short-links" <> "." <> scheme) meta of
    Just template -> fillTemplate template path
    Nothing -> do
      binding :: Text <-
        lookupMeta ("short-links" <> "." <> "bind" <> "." <> scheme) meta
      template <-
        lookupMeta ("short-links" <> "." <> scheme <> "." <> binding) meta
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
