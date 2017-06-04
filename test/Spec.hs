{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Yaml as Y
import NeatInterpolation
import Project as P
import Student
import System.FilePath
import System.FilePath.Glob
import System.FilePath.Posix
import Text.Pandoc
import Utilities

main = do
  projectDir <- calcProjectDirectory
  --
  let publicDir = projectDir </> "public"
  let cacheDir = publicDir </> "cache"
  let supportDir = publicDir </> "support"
  --
  metaFiles <- globDir1 (compile "**/*-meta.yaml") projectDir
  print metaFiles
  genStudentData projectDir
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
        adjustLocalUrl projectDir "base" "http://heise.de" `shouldBe`
          "http://heise.de"
                 --
        adjustLocalUrl projectDir "base" "/some/where" `shouldBe` projectDir </>
          "some/where"
                 --
        adjustLocalUrl projectDir "base" "some/where" `shouldBe`
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
        P.removeCommonPrefix ("fasel/bla", "fasel/bla/lall") `shouldBe` ("", "lall")
        P.removeCommonPrefix ("lurgel/hopp", "fasel/bla/lall") `shouldBe` ("lurgel/hopp", "fasel/bla/lall")
        P.removeCommonPrefix ("/lurgel/hopp", "fasel/bla/lall") `shouldBe` ("/lurgel/hopp", "fasel/bla/lall")
        P.removeCommonPrefix ("/lurgel/hopp", "/fasel/bla/lall") `shouldBe` ("lurgel/hopp", "fasel/bla/lall")
    --
    describe "cacheRemoteFile" $
      it
        "Stores the data behind a URL locally, if possible. Return the local path to the cached file." $ do
        cacheRemoteFile
          cacheDir
          "https://tramberend.beuth-hochschule.de/img/htr-beuth.jpg" `shouldReturn`
          cacheDir </>
          "bc137c359488beadbb61589f7fe9e208.jpg"
        cacheRemoteFile
          cacheDir
          "ftp://tramberend.beuth-hochschule.de/img/htr-beuth.jpg" `shouldReturn`
          "ftp://tramberend.beuth-hochschule.de/img/htr-beuth.jpg"
        cacheRemoteFile cacheDir "/img/htr-beuth.jpg" `shouldReturn`
          "/img/htr-beuth.jpg"
        cacheRemoteFile cacheDir "img/htr-beuth.jpg" `shouldReturn`
          "img/htr-beuth.jpg"
       --
    describe "cacheRemoteImages" $
      it
        "Replaces all remote images in the pandoc document with locally caches copies." $
      cacheRemoteImages
        cacheDir
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
                (cacheDir </> "bc137c359488beadbb61589f7fe9e208.jpg", "")
            ]
        ]
       --
    describe "parseStudentData" $
      it "Parses student data in YAML format into a nifty data structure." $
      parseStudentData projectDir `shouldReturn` Just realData

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
