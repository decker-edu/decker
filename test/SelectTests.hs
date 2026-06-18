module SelectTests (selectTests) where

import Data.Text (Text)
import Test.Hspec
import Text.Decker.Filter.Select (dropSolutionContent)
import Text.Decker.Internal.Meta (addMetaKeyValue)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)

upcomingMeta :: Meta
upcomingMeta =
  addMetaKeyValue "lecture.publish" ("yes" :: Text) $
    addMetaKeyValue "lecture.status" ("upcoming" :: Text) nullMeta

doneMeta :: Meta
doneMeta =
  addMetaKeyValue "lecture.publish" ("yes" :: Text) $
    addMetaKeyValue "lecture.status" ("done" :: Text) nullMeta

-- A deck with a solution box (level-2 header + content) and a solution slide
-- (level-1 header), plus regular content.
sampleBlocks :: [Block]
sampleBlocks =
  [ Header 1 ("", [], []) [Str "SlideA"],
    Header 2 ("", ["solution"], []) [Str "SolBox"],
    Para [Str "secret-box"],
    Header 2 ("", [], []) [Str "NormalBox"],
    Para [Str "public-box"],
    Header 1 ("", ["solution"], []) [Str "SolSlide"],
    Para [Str "secret-slide"],
    Header 1 ("", [], []) [Str "SlideB"]
  ]

strings :: Pandoc -> [Text]
strings = query f
  where
    f (Str s) = [s]
    f _ = []

selectTests :: SpecWith ()
selectTests =
  describe "dropSolutionContent" $ do
    it "drops solution slides and boxes when publishing an upcoming lecture" $ do
      let out = strings (dropSolutionContent upcomingMeta (Pandoc nullMeta sampleBlocks))
      out `shouldSatisfy` notElem "secret-box"
      out `shouldSatisfy` notElem "SolSlide"
      out `shouldSatisfy` notElem "secret-slide"
      out `shouldSatisfy` elem "SlideA"
      out `shouldSatisfy` elem "NormalBox"
      out `shouldSatisfy` elem "public-box"
      out `shouldSatisfy` elem "SlideB"
    it "keeps all content for a done lecture" $ do
      let out = strings (dropSolutionContent doneMeta (Pandoc nullMeta sampleBlocks))
      out `shouldSatisfy` elem "secret-box"
      out `shouldSatisfy` elem "SolSlide"
      out `shouldSatisfy` elem "secret-slide"
    it "keeps all content when not publishing (no lecture.publish)" $ do
      let out = strings (dropSolutionContent nullMeta (Pandoc nullMeta sampleBlocks))
      out `shouldSatisfy` elem "secret-box"
      out `shouldSatisfy` elem "SolSlide"
