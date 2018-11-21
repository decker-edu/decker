module WatchTests
  ( watchTests
  ) where

import Glob
import Test.Hspec

watchTests = do
  describe "fastGlobFiles" $ do
    it "globs fastest if no extensions are specified" $
      fastGlobFiles [] [] "test" `shouldReturn` []
    it "returns all Haskell source files if the extensions include '.hs'" $
      fastGlobFiles [] [".hs"] "test" `shouldReturn`
      ["test/WatchTests.hs", "test/Spec.hs"]
    it "globs just one file if root is a single file" $
      fastGlobFiles [] [".hs"] "test/Spec.hs" `shouldReturn` ["test/Spec.hs"]
    it "does not descend into excluded dirs" $
      fastGlobFiles ["example", "support", "template"] [".html"] "resource" `shouldReturn`
      []
  describe "fastGlobDirs" $ do
    it "globs for directories" $
      fastGlobDirs ["include"] "resource/example" `shouldReturn`
      ["resource/example", "resource/example/img"]
