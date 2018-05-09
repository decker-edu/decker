import Test.Hspec

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Yaml as Y
import Filter
import Project as P
import qualified System.Directory as Dir
import System.FilePath
import System.FilePath.Glob
import System.FilePath
import Text.Pandoc
import Utilities

main = do
  dirs <- projectDirectories
  --
  deckTemplate <- B.readFile (project dirs </> "resource/template/deck.html")
  --
  metaFiles <- globDir1 (compile "**/*-meta.yaml") (project dirs)
  print metaFiles
  --
  hspec $
  --
   do
    describe "makeRelativeTo" $
      it "calculates the path of file relative to dir. Inlcudes '..'" $ do
        makeRelativeTo "" "img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two" "/one/two/img.png" `shouldBe` "img.png"
        makeRelativeTo "/one/two/three" "/one/two/four/img.png" `shouldBe`
          "../four/img.png"
        makeRelativeTo "/some/where/else" "/one/two/four/img.png" `shouldBe`
          "../../../one/two/four/img.png"
        makeRelativeTo
          "/Users/henrik/tmp/decker-demo/public"
          "/Users/henrik/tmp/decker-demo/public/cache/b48cadafb942dc1426316772321dd0c7.png" `shouldBe`
          "cache/b48cadafb942dc1426316772321dd0c7.png"
    --
    describe "removeCommonPrefix" $
      it "removes the common prefix from two pathes." $ do
        P.removeCommonPrefix ("", "") `shouldBe` ("", "")
        P.removeCommonPrefix ("fasel/bla", "fasel/bla/lall") `shouldBe`
          ("", "lall")
        P.removeCommonPrefix ("lurgel/hopp", "fasel/bla/lall") `shouldBe`
          ("lurgel/hopp", "fasel/bla/lall")
        P.removeCommonPrefix ("/lurgel/hopp", "fasel/bla/lall") `shouldBe`
          ("/lurgel/hopp", "fasel/bla/lall")
        P.removeCommonPrefix ("/lurgel/hopp", "/fasel/bla/lall") `shouldBe`
          ("lurgel/hopp", "fasel/bla/lall")
    --
    describe "copyResource" $
      it
        "copies an existing resource to the public dir and returns the public URL." $ do
        Dir.doesFileExist
          ((project dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
        copyResource
          (Resource
             ((project dirs) </> "resource/example/img/06-metal.png")
             ((public dirs) </> "resource/example/img/06-metal.png")
             "img/06-metal.png") `shouldReturn`
          "img/06-metal.png"
        Dir.doesFileExist
          ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
    --
    describe "linkResource" $
      it
        "links an existing resource to the public dir and returns the public URL." $ do
        Dir.doesFileExist
          ((project dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
        linkResource
          (Resource
             ((project dirs) </> "resource/example/img/06-metal.png")
             ((public dirs) </> "resource/example/img/06-metal.png")
             "img/06-metal.png") `shouldReturn`
          "img/06-metal.png"
        Dir.pathIsSymbolicLink
          ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
    -- 
    describe "transformImageSize" $
      it
        "transfers 'width' and 'height' attribute values to css style values and add them to the 'style' attribute value." $ do
        transformImageSize [("width", "100%")] `shouldBe`
          [("style", "height:auto;width:100%;")]
        transformImageSize [("height", "50%")] `shouldBe`
          [("style", "height:50%;width:auto;")]
        transformImageSize [("width", "100%"), ("style", "color:red;")] `shouldBe`
          [("style", "height:auto;width:100%;color:red;")]
