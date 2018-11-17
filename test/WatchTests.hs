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
    it "does not descend into excluded dirs" $
      fastGlob ["example", "support", "template"] [".html"] "resource" `shouldReturn`
      []
