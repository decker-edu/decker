module WatchTests
  ( watchTests
  ) where

import Data.List
import Test.Hspec
import Text.Decker.Project.Glob

watchTests =
  describe "fastGlobFiles" $ do
    it "returns all Haskell source files if the extensions include '.hs'" $
      sort <$>
      fastGlobFiles [] [".hs"] "test" `shouldReturn`
      sort
        [ "test/MetaTests.hs"
        , "test/ShortLinkTests.hs"
        , "test/Spec.hs"
        , "test/WatchTests.hs"
        ]
    it "globs just one file if root is a single file" $
      fastGlobFiles [] [".hs"] "test/Spec.hs" `shouldReturn` ["test/Spec.hs"]
