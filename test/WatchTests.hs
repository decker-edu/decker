module WatchTests
  ( watchTests
  ) where

import Project
import Test.Hspec

watchTests =
  describe "fastGlob" $ do
    it "globs fastest if no extensions are specified" $
      fastGlob [] [] "test" `shouldReturn` []
    it "returns all Haskell source files if the extensions include '.hs'" $
      fastGlob [] [".hs"] "test" `shouldReturn`
      ["test/WatchTests.hs", "test/Spec.hs"]
    it "globs just one file if root is a single file" $
      fastGlob [] [".hs"] "test/Spec.hs" `shouldReturn` ["test/Spec.hs"]
    it "returns all HTML files if the extensions include '.html'" $
      fastGlob [] [".html"] "test" `shouldReturn`
      [ "test/decks/figure-deck.html"
      , "test/decks/mc/webgl_geometry_minecraft_ao.html"
      , "test/decks/column-deck.html"
      , "test/reload.html"
      ]
    it "returns all sources if the extensions includes '-deck.md'" $
      fastGlob [] ["-deck.md"] "test" `shouldReturn`
      [ "test/decks/background-image-deck.md"
      , "test/decks/menu-deck.md"
      , "test/decks/rendered-code-deck.md"
      , "test/decks/bibliography-deck.md"
      , "test/decks/meta-resources-deck.md"
      , "test/decks/media-deck.md"
      , "test/decks/video-autoplay-deck.md"
      , "test/decks/slide-separation-deck.md"
      , "test/decks/iframe-deck.md"
      , "test/decks/background-video-deck.md"
      , "test/decks/include-deck.md"
      , "test/decks/css-deck.md"
      , "test/decks/column-deck.md"
      , "test/decks/macro-deck.md"
      , "test/decks/handout-deck.md"
      , "test/decks/figure-deck.md"
      ]
    it "does not descend into excluded dirs" $
      fastGlob ["test/decks"] [".html"] "test" `shouldReturn`
      ["test/reload.html"]
