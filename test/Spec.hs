import IncludeTests
import MediaTests
import MetaTests
import Test.Hspec
-- Import WatchTests

--  import ShortLinkTests

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as B
import System.FilePath
-- import System.FilePath.Glob
import Text.Decker.Project.Project as P

main = do
  dirs <- projectDirectories
  --
  deckTemplate <- B.readFile (dirs ^. project </> "resource/template/deck.html")
  --
  hspec $
  -- TODO reenable all tests
   do
    mediaTests
    includeTests
    -- shortLinkTests
    -- watchTests
    -- sketchTests
    metaTests
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
        P.removeCommonPrefix ("", "") `shouldBe` ("", "")
        P.removeCommonPrefix ("fasel/bla", "fasel/bla/lall") `shouldBe`
          ("", "lall")
        P.removeCommonPrefix ("lurgel/hopp", "fasel/bla/lall") `shouldBe`
          (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        P.removeCommonPrefix ("/lurgel/hopp", "fasel/bla/lall") `shouldBe`
          (joinPath ["/lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])
        P.removeCommonPrefix ("/lurgel/hopp", "/fasel/bla/lall") `shouldBe`
          (joinPath ["lurgel", "hopp"], joinPath ["fasel", "bla", "lall"])

