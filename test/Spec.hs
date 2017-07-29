{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Yaml as Y
import Project as P
import Project
import qualified System.Directory as Dir
import System.FilePath
import System.FilePath.Glob
import System.FilePath.Posix
import Text.Pandoc
import Utilities

main = do
  dirs <- projectDirectories
  --
  deckTemplate <- B.readFile ((project dirs) </> "resource/template/deck.html")
  --
  metaFiles <- globDir1 (compile "**/*-meta.yaml") (project dirs)
  print metaFiles
  --
  hspec $
  --
   do
    describe "isCacheableURI" $
      it "returns True if URL has http: or https: protocol" $ do
        isCacheableURI "http://heise.de" `shouldBe` True
        isCacheableURI "ftp://heise.de" `shouldBe` False
    --
    describe "adjustLocalUrl" $
      it
        "adjusts URL to be relative to the project root or the provided base directory" $ do
        adjustLocalUrl (project dirs) "base" "http://heise.de" `shouldBe`
          "http://heise.de"
                 --
        adjustLocalUrl (project dirs) "base" "/some/where" `shouldBe`
          (project dirs) </>
          "some/where"
                 --
        adjustLocalUrl (project dirs) "base" "some/where" `shouldBe`
          "base/some/where"
       --
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
      it "Removes the common prefix from two pathes." $ do
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
    describe "resolveLocally" $
      it "Resolves a file path to a concrete verified file system path." $ do
        (resolveLocally
           (project dirs)
           ((project dirs) </> "resource/example")
           "img/06-metal.png") `shouldReturn`
          (Just ((project dirs) </> "resource/example/img/06-metal.png"))
        (resolveLocally
           (project dirs)
           ((project dirs) </> "resource/example")
           "img/06-metal.png") `shouldReturn`
          Just ((project dirs) </> "resource/example/img/06-metal.png")
        (resolveLocally
           (project dirs)
           ((project dirs) </> "resource/example")
           "img/07-metal.png") `shouldReturn`
          Nothing
    --
    describe "copyResource" $
      it
        "Copies an existing resource to the public dir and returns the public URL." $ do
        Dir.doesFileExist ((project dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
        copyResource
          (Resource
             ((project dirs) </> "resource/example/img/06-metal.png")
             ((public dirs) </> "resource/example/img/06-metal.png")
             "img/06-metal.png") `shouldReturn`
          "img/06-metal.png"
        Dir.doesFileExist ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
    --
    describe "linkResource" $
      it
        "Links an existing resource to the public dir and returns the public URL." $ do
        Dir.doesFileExist ((project dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
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
    describe "provisionResource" $ do
      it "Copies a presentation time resource into the public dir." $ do
        provisionResource
          Copy
          dirs
          ((project dirs) </> "resource/example")
          "img/06-metal.png" `shouldReturn`
          "img/06-metal.png"
        Dir.doesFileExist ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
      it "Links a presentation time resource into the public dir." $ do
        provisionResource
          SymbolicLink
          dirs
          ((project dirs) </> "resource/example")
          "img/06-metal.png" `shouldReturn`
          "img/06-metal.png"
        Dir.doesFileExist ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
        Dir.pathIsSymbolicLink
          ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
      it "Throws, if the resource can not be found." $ do
        provisionResource
          Copy
          dirs
          ((project dirs) </> "resource/example")
          "img/does-not-exist.png" `shouldThrow`
          anyException
    --
    describe "findFile" $ do
      it "Finds local file system resources that sre needed at compile time." $ do
        findFile (project dirs) (project dirs) "resource/template/deck.html" `shouldReturn`
          (project dirs) </>
          "resource/template/deck.html"
      it "Throws, if the resource can not be found." $ do
        findFile (project dirs) (project dirs) "deck.html" `shouldThrow`
          anyException
    --
    describe "readResource" $ do
      it
        "Finds local file system or built-in resources that sre needed at compile time." $ do
        readResource
          (project dirs)
          ((project dirs) </> "resource/template")
          "deck.html" `shouldReturn`
          deckTemplate
        readResource (project dirs) (project dirs) "deck.html" `shouldReturn`
          deckTemplate
      it "Throws, if the resource can not be read." $ do
        readResource (project dirs) (project dirs) "dreck.html" `shouldThrow`
          anyException
    --
    describe "cacheRemoteFile" $
      it
        "Stores the data behind a URL locally, if possible. Return the local path to the cached file." $ do
        cacheRemoteFile
          (cache dirs)
          "https://tramberend.beuth-hochschule.de/img/htr-beuth.jpg" `shouldReturn`
          (cache dirs) </>
          "bc137c359488beadbb61589f7fe9e208.jpg"
        cacheRemoteFile
          (cache dirs)
          "ftp://tramberend.beuth-hochschule.de/img/htr-beuth.jpg" `shouldReturn`
          "ftp://tramberend.beuth-hochschule.de/img/htr-beuth.jpg"
        cacheRemoteFile (cache dirs) "/img/htr-beuth.jpg" `shouldReturn`
          "/img/htr-beuth.jpg"
        cacheRemoteFile (cache dirs) "img/htr-beuth.jpg" `shouldReturn`
          "img/htr-beuth.jpg"
       --
    describe "cacheRemoteImages" $
      it
        "Replaces all remote images in the pandoc document with locally caches copies." $
      cacheRemoteImages
        (cache dirs)
        (Pandoc
           nullMeta
           [ Para
               [ Image
                   nullAttr
                   []
                   ( "https://tramberend.beuth-hochschule.de/img/htr-beuth.jpg"
                   , "")
               ]
           ]) `shouldReturn`
      Pandoc
        nullMeta
        [ Para
            [ Image
                nullAttr
                []
                ((cache dirs) </> "bc137c359488beadbb61589f7fe9e208.jpg", "")
            ]
        ]
