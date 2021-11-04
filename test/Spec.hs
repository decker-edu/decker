{-# LANGUAGE OverloadedStrings #-}

import Text.Decker.Internal.Helper as H
import Text.Decker.Internal.URI

import WatchTests
import ShortLinkTests
import IncludeTests
import MediaTests
import MetaTests

import System.FilePath.Posix
import GHC.IO.Encoding
import qualified Data.ByteString.Char8 as B
import Test.Hspec

main = do
  setLocaleEncoding utf8
  deckTemplate <- B.readFile "resource/decker/template/deck.html"
       --
  hspec $ do
    shortLinkTests
    includeTests
    mediaTests
    metaTests
    watchTests
    describe "makeRelativeTo" $
      it "calculates the path of file relative to dir. Includes '..'" $ do
        makeRelativeTo "" "img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two" "/one/two/img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two/three" "/one/two/four/img.png" `shouldBe`
          joinPath ["..", "four", "img.png"]
        makeRelativeTo "/some/where/else" "/one/two/four/img.png" `shouldBe`
          joinPath ["..", "..", "..", "one", "two", "four", "img.png"]
        makeRelativeTo
          "/Users/henrik/tmp/decker-demo/public"
          "/Users/henrik/tmp/decker-demo/public/cache/b48cadafb942dc1426316772321dd0c7.png" `shouldBe`
          joinPath ["cache", "b48cadafb942dc1426316772321dd0c7.png"]
              --
    describe "removeCommonPrefix" $
      it "removes the common prefix from two pathes." $ do
        H.removeCommonPrefix ("", "") `shouldBe` ("", "")
        H.removeCommonPrefix ("fasel/bla", "fasel/bla/lall") `shouldBe`
          ("", "lall")
        H.removeCommonPrefix ("lurgel/hopp", "fasel/bla/lall") `shouldBe`
          (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        H.removeCommonPrefix ("/lurgel/hopp", "fasel/bla/lall") `shouldBe`
          (joinPath ["/lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        H.removeCommonPrefix ("/lurgel/hopp", "/fasel/bla/lall") `shouldBe`
          (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
    describe "makeProjectPath" $
      it "creates an absolute path" $ do
        makeProjectPath "base" "/this" `shouldBe`
          "this"
        makeProjectPath "base" "this" `shouldBe`
          "base/this"
