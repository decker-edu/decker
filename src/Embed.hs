{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE TemplateHaskell #-}

module Embed
  ( deckerHelpText
  , deckerExampleDir
  , deckerSupportDir
  , deckerTemplateDir
  , deckTemplate
  , pageTemplate
  , pageLatexTemplate
  , examLatexTemplate
  , handoutTemplate
  , handoutLatexTemplate
  , testerMultipleChoiceTemplate
  , testerMultipleAnswersTemplate
  , testerFillTextTemplate
  , testerFreeFormTemplate
  , testLatexTemplate
  , defaultTemplate
  , defaultTemplateString
  ) where

import qualified Data.ByteString.Char8 as B
import Data.FileEmbed
import Data.List
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

examLatexTemplate = fromJust $ defaultTemplateString "exam.tex"

handoutTemplate = fromJust $ defaultTemplateString "handout.html"

handoutLatexTemplate = fromJust $ defaultTemplateString "handout.tex"

testerMultipleChoiceTemplate =
  fromJust $ defaultTemplate "mc-quest-catalog-template.md"

testerMultipleAnswersTemplate =
  fromJust $ defaultTemplate "ma-quest-catalog-template.md"

testerFillTextTemplate =
  fromJust $ defaultTemplate "ft-quest-catalog-template.md"

testerFreeFormTemplate =
  fromJust $ defaultTemplate "ff-quest-catalog-template.md"

testLatexTemplate = fromJust $ defaultTemplate "test.tex"
