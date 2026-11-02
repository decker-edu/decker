module NotesTests (notesTests) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Development.Shake
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Text.Decker.Filter.Filter (processPandoc, processSlides)
import Text.Decker.Internal.Common (htmlDeck, pandocReaderOpts)
import Text.Pandoc
import Text.Pandoc.Walk (query)

-- | Reads Markdown with decker's reader options and runs the slide filters
-- for an HTML deck. The filters live in the Decker monad, which needs a Shake
-- action to run.
slides :: Text -> IO Pandoc
slides markdown = do
  pandoc <- runIOorExplode $ readMarkdown pandocReaderOpts markdown
  result <- newIORef Nothing
  withSystemTempDirectory "decker-notes-test" $ \dir ->
    shake shakeOptions {shakeFiles = dir, shakeVerbosity = Silent} $
      action $ do
        out <- processPandoc processSlides dir htmlDeck pandoc
        liftIO $ writeIORef result (Just out)
  readIORef result >>= maybe (fail "processSlides did not run") return

isNotes :: Attr -> Bool
isNotes (_, cls, kvs) = "notes" `elem` cls && lookup "data-tag" kvs == Just "aside"

-- | All words inside speaker notes asides.
inNotes :: Pandoc -> [Text]
inNotes = query notes
  where
    notes (Div attr blocks) | isNotes attr = query str blocks
    notes _ = []

-- | All words outside speaker notes asides, i.e. visible on the slide.
onSlide :: Pandoc -> [Text]
onSlide (Pandoc _ blocks) = concatMap visible blocks
  where
    visible (Div attr bs)
      | isNotes attr = []
      | otherwise = concatMap visible bs
    visible block = query str block

str :: Inline -> [Text]
str (Str s) = [s]
str _ = []

-- | Header levels of all headers inside speaker notes asides, with their text.
noteHeaders :: Pandoc -> [(Int, Text)]
noteHeaders = query notes
  where
    notes (Div attr blocks) | isNotes attr = query header blocks
    notes _ = []
    header (Header n _ text) = [(n, Text.unwords (query str text))]
    header _ = []

notesTests :: SpecWith ()
notesTests =
  describe "speaker notes" $ do
    it "converts a top-level `## {.notes}` section" $ do
      out <- slides "# Slide\n\nVisible\n\n## Note {.notes}\n\nHidden\n"
      inNotes out `shouldContain` ["Hidden"]
      onSlide out `shouldNotContain` ["Hidden"]
      onSlide out `shouldContain` ["Visible"]
    it "converts a `## {.notes}` section after a `. . .` pause" $ do
      out <- slides "# Slide\n\nFirst\n\n. . .\n\nSecond\n\n## Note {.notes}\n\nHidden\n"
      inNotes out `shouldContain` ["Hidden"]
      onSlide out `shouldNotContain` ["Hidden"]
      onSlide out `shouldContain` ["Second"]
    it "converts a `## {.notes}` section inside a fenced div" $ do
      out <- slides "# Slide\n\n::: foo\nVisible\n\n## Note {.notes}\n\nHidden\n:::\n"
      inNotes out `shouldContain` ["Hidden"]
      onSlide out `shouldNotContain` ["Hidden"]
      onSlide out `shouldContain` ["Visible"]
    it "ends a `## {.notes}` section at a `. . .` pause" $ do
      out <- slides "# Slide\n\n## Note {.notes}\n\nHidden\n\n. . .\n\nLater\n"
      inNotes out `shouldContain` ["Hidden"]
      inNotes out `shouldNotContain` ["Later"]
      onSlide out `shouldContain` ["Later"]
    it "demotes headers of a `## {.notes}` section once" $ do
      out <- slides "# Slide\n\n## Note {.notes}\n\n### Sub\n\nHidden\n"
      noteHeaders out `shouldBe` [(3, "Note"), (4, "Sub")]
    it "demotes headers of a `::: notes` div once" $ do
      out <- slides "# Slide\n\n::: notes\n### Sub\n\nHidden\n:::\n"
      noteHeaders out `shouldBe` [(4, "Sub")]
    it "demotes headers of a `# {.notes}` slide once" $ do
      out <- slides "# Slide\n\nVisible\n\n# Notes {.notes}\n\n## Box\n\nHidden\n"
      noteHeaders out `shouldBe` [(2, "Notes"), (3, "Box")]
      onSlide out `shouldNotContain` ["Hidden"]
