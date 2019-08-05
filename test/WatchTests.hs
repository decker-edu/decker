module WatchTests
  ( watchTests
  ) where

import Data.List
import Text.Decker.Project.Glob
import Test.Hspec

watchTests =
  describe "fastGlobFiles" $ do
    it "globs fastest if no extensions are specified" $
      fastGlobFiles [] [] "test" `shouldReturn` []
    it "returns all Haskell source files if the extensions include '.hs'" $
      fastGlobFiles [] [".hs"] "test" `shouldReturn`
      sort
        [ "test/SketchTests.hs"
        , "test/WatchTests.hs"
        , "test/Spec.hs"
        , "test/IncludeTests.hs"
        ]
    it "globs just one file if root is a single file" $
      fastGlobFiles [] [".hs"] "test/Spec.hs" `shouldReturn` ["test/Spec.hs"]
    it "does not descend into excluded dirs" $
      fastGlobFiles ["example", "support", "template"] [".html"] "resource" `shouldReturn`
      []
