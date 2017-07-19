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
import Student
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.FilePath.Posix
import Text.Pandoc
import Utilities

main = do
  dirs <- projectDirectories
  --
  metaFiles <- globDir1 (compile "**/*-meta.yaml") (project dirs)
  print metaFiles
  genStudentData (project dirs)
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
        doesFileExist ((project dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
        copyResource
          (Resource
             ((project dirs) </> "resource/example/img/06-metal.png")
             ((public dirs) </> "resource/example/img/06-metal.png")
             "img/06-metal.png") `shouldReturn`
          "img/06-metal.png"
        doesFileExist ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
    --
    describe "linkResource" $
      it
        "Copies an existing resource to the public dir and returns the public URL." $ do
        doesFileExist ((project dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
        linkResource
          (Resource
             ((project dirs) </> "resource/example/img/06-metal.png")
             ((public dirs) </> "resource/example/img/06-metal.png")
             "img/06-metal.png") `shouldReturn`
          "img/06-metal.png"
        pathIsSymbolicLink
          ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
    -- 
    describe "provisionResource" $
      it "Resolves a file path to a verified file system path." $ do
        provisionResource
          SymbolicLink
          dirs
          ((project dirs) </> "resource/example")
          "img/06-metal.png" `shouldReturn`
          "img/06-metal.png"
        doesFileExist ((public dirs) </> "resource/example/img/06-metal.png") `shouldReturn`
          True
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
       --
    describe "parseStudentData" $
      it "Parses student data in YAML format into a nifty data structure." $
      parseStudentData (project dirs) `shouldReturn` Just realData

mockData :: Students
mockData =
  Students
  { stds_course = "Mock"
  , stds_semester = "Mock 1234"
  , stds_students =
      H.fromList
        [ ("888888", Student "mock" "mock" "mock" "mock" "mock" "mock" 1)
        , ("888889", Student "mock" "mock" "mock" "mock" "mock" "mock" 1)
        ]
  }

realData :: Students
realData =
  Students
  { stds_course = "Mock"
  , stds_semester = "Mock 1234"
  , stds_students =
      (H.fromList
         [ ( "836381"
           , Student
             { std_displayName = "Justen, David Alexander"
             , std_employeeNumber = "836381"
             , std_givenName = "David Alexander"
             , std_mail = "s64386@beuth-hochschule.de"
             , std_sAMAccountName = "s64386"
             , std_sn = "Justen"
             , std_track = 1
             })
         , ( "798101"
           , Student
             { std_displayName = "Mahmoud, Hassan"
             , std_employeeNumber = "798101"
             , std_givenName = "Hassan"
             , std_mail = "s53445@beuth-hochschule.de"
             , std_sAMAccountName = "s53445"
             , std_sn = "Mahmoud"
             , std_track = 1
             })
         , ( "814510"
           , Student
             { std_displayName = "Sahli, Hanen"
             , std_employeeNumber = "814510"
             , std_givenName = "Hanen"
             , std_mail = "s57637@beuth-hochschule.de"
             , std_sAMAccountName = "s57637"
             , std_sn = "Sahli"
             , std_track = 1
             })
         , ( "832701"
           , Student
             { std_displayName = "Naci Aydogan"
             , std_employeeNumber = "832701"
             , std_givenName = "Naci"
             , std_mail = "s61660@beuth-hochschule.de"
             , std_sAMAccountName = "s61660"
             , std_sn = "Aydogan"
             , std_track = 1
             })
         ])
  }

genStudentData :: FilePath -> IO ()
genStudentData dir =
  Y.encodeFile (dir </> "test/student-mock-data.yaml") mockData

parseStudentData :: FilePath -> IO (Maybe Students)
parseStudentData dir = do
  Y.decodeFile $ dir </> "test/student-test-data.yaml"
