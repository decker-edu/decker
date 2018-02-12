{-- Author: Henrik Tramberend <henrik@tramberend.de> --}

module Embed
  ( deckerHelpText
  , deckerExampleDir
  , deckerSupportDir
  , deckerTemplateDir
  , deckTemplate
  , pageTemplate
  , pageLatexTemplate
  , handoutTemplate
  , handoutLatexTemplate
  , defaultTemplate
  , defaultTemplateString
  ) where

import qualified Data.ByteString.Char8 as B
import Data.FileEmbed
import Data.List
import Data.List.Extra
import Data.Maybe

deckerExampleDir :: [(FilePath, B.ByteString)]
deckerExampleDir = $(makeRelativeToProject "resource/example" >>= embedDir)

deckerSupportDir :: [(FilePath, B.ByteString)]
deckerSupportDir = $(makeRelativeToProject "resource/support" >>= embedDir)

deckerTemplateDir :: [(FilePath, B.ByteString)]
deckerTemplateDir = $(makeRelativeToProject "resource/template" >>= embedDir)

defaultTemplate :: FilePath -> Maybe B.ByteString
defaultTemplate path = snd <$> find (\(k, _) -> k == path) deckerTemplateDir

defaultTemplateString :: FilePath -> Maybe String
defaultTemplateString path = B.unpack <$> defaultTemplate path

deckerHelpText = fromJust $ defaultTemplateString "help-page.md"

deckTemplate = fromJust $ defaultTemplateString "deck.html"

pageTemplate = fromJust $ defaultTemplateString "page.html"

pageLatexTemplate = fromJust $ defaultTemplateString "page.tex"

handoutTemplate = fromJust $ defaultTemplateString "handout.html"

handoutLatexTemplate = fromJust $ defaultTemplateString "handout.tex"
