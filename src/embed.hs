{-# LANGUAGE TemplateHaskell #-}

module Embed
       (deckerHelpText, deckerExampleDir, deckerSupportDir, deckTemplate,
        pageTemplate, pageLatexTemplate, handoutTemplate,
        handoutLatexTemplate, testerMultipleChoiceTemplate,
        testerFillTextTemplate, testerFreeFormTemplate, testLatexTemplate)
       where

import Data.FileEmbed
import qualified Data.ByteString.Char8 as B

deckerExampleDir :: [(FilePath, B.ByteString)]
deckerExampleDir = $(makeRelativeToProject "resource/example" >>= embedDir)

deckerSupportDir :: [(FilePath, B.ByteString)]
deckerSupportDir = $(makeRelativeToProject "resource/support" >>= embedDir)

deckerHelpText :: String
deckerHelpText =
  B.unpack $(makeRelativeToProject "resource/help-page.md" >>= embedFile)

deckTemplate :: String
deckTemplate =
  B.unpack $(makeRelativeToProject "resource/deck.html" >>= embedFile)

pageTemplate :: String
pageTemplate =
  B.unpack $(makeRelativeToProject "resource/page.html" >>= embedFile)

pageLatexTemplate :: String
pageLatexTemplate =
  B.unpack $(makeRelativeToProject "resource/page.tex" >>= embedFile)

handoutTemplate :: String
handoutTemplate =
  B.unpack $(makeRelativeToProject "resource/handout.html" >>= embedFile)

handoutLatexTemplate :: String
handoutLatexTemplate =
  B.unpack $(makeRelativeToProject "resource/handout.tex" >>= embedFile)

testerMultipleChoiceTemplate :: B.ByteString
testerMultipleChoiceTemplate =
  $(makeRelativeToProject "resource/mc-quest-catalog-template.md" >>= embedFile)

testerFillTextTemplate :: B.ByteString
testerFillTextTemplate =
  $(makeRelativeToProject "resource/ft-quest-catalog-template.md" >>= embedFile)

testerFreeFormTemplate :: B.ByteString
testerFreeFormTemplate =
  $(makeRelativeToProject "resource/ff-quest-catalog-template.md" >>= embedFile)

testLatexTemplate :: B.ByteString
testLatexTemplate =
  $(makeRelativeToProject "resource/test.tex" >>= embedFile)
