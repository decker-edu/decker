import Data.ByteString.Char8 qualified as B
import GHC.IO.Encoding
import MetaTests
import ShortLinkTests
import System.FilePath.Posix
import Test.Hspec
import Text.Decker.Internal.Helper as H
import Text.Decker.Internal.URI
import Text.Decker.Server.Video
import WatchTests

main = do
  setLocaleEncoding utf8
  deckTemplate <- B.readFile "resource/decker/template/deck.html"
  --
  hspec $ do
    shortLinkTests
    metaTests
    watchTests
    describe "makeRelativeTo" $
      it "calculates the path of file relative to dir. Includes '..'" $ do
        makeRelativeTo "" "img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two" "/one/two/img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two/three" "/one/two/four/img.png"
          `shouldBe` joinPath ["..", "four", "img.png"]
        makeRelativeTo "/some/where/else" "/one/two/four/img.png"
          `shouldBe` joinPath ["..", "..", "..", "one", "two", "four", "img.png"]
        makeRelativeTo
          "/Users/henrik/tmp/decker-demo/public"
          "/Users/henrik/tmp/decker-demo/public/cache/b48cadafb942dc1426316772321dd0c7.png"
          `shouldBe` joinPath ["cache", "b48cadafb942dc1426316772321dd0c7.png"]
    --
    describe "removeCommonPrefix" $
      it "removes the common prefix from two pathes." $ do
        H.removeCommonPrefix ("", "") `shouldBe` ("", "")
        H.removeCommonPrefix ("fasel/bla", "fasel/bla/lall")
          `shouldBe` ("", "lall")
        H.removeCommonPrefix ("lurgel/hopp", "fasel/bla/lall")
          `shouldBe` (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        H.removeCommonPrefix ("/lurgel/hopp", "fasel/bla/lall")
          `shouldBe` (joinPath ["/lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        H.removeCommonPrefix ("/lurgel/hopp", "/fasel/bla/lall")
          `shouldBe` (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
    describe "makeProjectPath" $
      it "creates an absolute path" $ do
        makeProjectPath "base" "/this"
          `shouldBe` "this"
        makeProjectPath "base" "this"
          `shouldBe` "base/this"
    describe "getSequenceNumber" $ do
      it "Extracts the sequence number from a numbered file name" $ do
        getSequenceNumber "img-0.png" `shouldBe` Just 0
        getSequenceNumber "img-recording-0.png" `shouldBe` Just 0
        getSequenceNumber "img-recording-0815.png" `shouldBe` Just 815
        getSequenceNumber "img.png" `shouldBe` Nothing
    describe "setSequenceNumber" $ do
      it "Sets the sequence number on a file name" $ do
        setSequenceNumber 3 "img.png" `shouldBe` "img-3.png"
        setSequenceNumber 4 "img-1.png" `shouldBe` "img-4.png"
    describe "getHighestSequenceNumber" $ do
      it "Extracts the highest sequence number from a list of file names" $ do
        getHighestSequenceNumber ["img-0.png", "img-3.png", "img-17.png"] `shouldBe` 17
        getHighestSequenceNumber ["img-0.png", "img-3.png", "img.png"] `shouldBe` 3
