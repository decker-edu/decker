{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Test.Hspec

import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Text.Pandoc
import Utilities
import Student
import System.FilePath
import System.FilePath.Posix
import System.FilePath.Glob
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as Map
import NeatInterpolation

main =
  do projectDir <- calcProjectDirectory
     --
     let publicDir = projectDir </> "public"
     let cacheDir = publicDir </> "cache"
     let supportDir = publicDir </> "support"
     --
     metaFiles <- globDir1 (compile "**/*-meta.yaml") projectDir
     putStrLn $ show metaFiles
     genStudentData projectDir
     --
     hspec $
       --
       do describe "isCacheableURI" $
            do it "returns True if URL has http: or https: protocol" $
                 do isCacheableURI "http://heise.de" `shouldBe` True
                    isCacheableURI "ftp://heise.de" `shouldBe` False
          --
          describe "adjustLocalUrl" $
            do it "adjusts URL to be relative to the project root or the provided base directory" $
                 do adjustLocalUrl projectDir "base" "http://heise.de" `shouldBe`
                      "http://heise.de"
                    --
                    adjustLocalUrl projectDir "base" "/some/where" `shouldBe`
                      projectDir </>
                      "some/where"
                    --
                    adjustLocalUrl projectDir "base" "some/where" `shouldBe`
                      "base/some/where"
          --
          describe "makeRelativeTo" $
            do it "calculates the path of file relative to dir. Inlcudes '..'" $
                 do makeRelativeTo "" "img.png" `shouldBe` "img.png"
                    makeRelativeTo "/one/two" "/one/two/img.png" `shouldBe`
                      "img.png"
                    makeRelativeTo "/one/two/three" "/one/two/four/img.png" `shouldBe`
                      "../four/img.png"
                    makeRelativeTo "/some/where/else" "/one/two/four/img.png" `shouldBe`
                      "../../../one/two/four/img.png"
          --
          describe "cacheRemoteFile" $
            it "Stores the data behind a URL locally, if possible. Return the local path to the cached file." $
            do cacheRemoteFile cacheDir "https://tramberend.beuth-hochschule.de/img/htr-beuth.jpg" `shouldReturn`
                 cacheDir </>
                 "bc137c359488beadbb61589f7fe9e208.jpg"
               cacheRemoteFile cacheDir "ftp://tramberend.beuth-hochschule.de/img/htr-beuth.jpg" `shouldReturn`
                 "ftp://tramberend.beuth-hochschule.de/img/htr-beuth.jpg"
               cacheRemoteFile cacheDir "/img/htr-beuth.jpg" `shouldReturn`
                 "/img/htr-beuth.jpg"
               cacheRemoteFile cacheDir "img/htr-beuth.jpg" `shouldReturn`
                 "img/htr-beuth.jpg"
          --
          describe "cacheRemoteImages" $
            it "Replaces all remote images in the pandoc document with locally caches copies." $
            do cacheRemoteImages
                 cacheDir
                 (Pandoc nullMeta
                         [(Para [Image nullAttr
                                       []
                                       ("https://tramberend.beuth-hochschule.de/img/htr-beuth.jpg"
                                       ,"")])]) `shouldReturn`
                 (Pandoc nullMeta
                         [(Para [Image nullAttr
                                       []
                                       (cacheDir </>
                                        "bc137c359488beadbb61589f7fe9e208.jpg"
                                       ,"")])])
          --
          describe "parseStudentData" $
            it "Parses student data in YAML format into a nifty data structure." $
            do parseStudentData projectDir `shouldReturn`
                 Just realData

mockData :: Students
mockData = Students $ Map.fromList [ ("888888", Student "mock" "mock" "mock" "mock" "mock" "mock" "mock" "mock" 1)
                                   , ("888889", Student "mock" "mock" "mock" "mock" "mock" "mock" "mock" "mock" 1)]

realData :: Students
realData = Students (Map.fromList [("836381",Student {std_uid = "s64386", std_department = "FB6", std_displayName = "Justen, David Alexander", std_employeeNumber = "836381", std_givenName = "David Alexander", std_mail = "s64386@beuth-hochschule.de", std_sAMAccountName = "s64386", std_sn = "Justen", std_track = 1}),("798101",Student {std_uid = "s53445", std_department = "FB6", std_displayName = "Mahmoud, Hassan", std_employeeNumber = "798101", std_givenName = "Hassan", std_mail = "s53445@beuth-hochschule.de", std_sAMAccountName = "s53445", std_sn = "Mahmoud", std_track = 1}),("814510",Student {std_uid = "s57637", std_department = "FB6", std_displayName = "Sahli, Hanen", std_employeeNumber = "814510", std_givenName = "Hanen", std_mail = "s57637@beuth-hochschule.de", std_sAMAccountName = "s57637", std_sn = "Sahli", std_track = 1}),("832701",Student {std_uid = "s61660", std_department = "FB6", std_displayName = "Naci Aydogan", std_employeeNumber = "832701", std_givenName = "Naci", std_mail = "s61660@beuth-hochschule.de", std_sAMAccountName = "s61660", std_sn = "Aydogan", std_track = 1})])

genStudentData :: FilePath -> IO ()
genStudentData dir = Y.encodeFile (dir </> "test/student-mock-data.yaml") mockData

parseStudentData :: FilePath -> IO (Maybe Students)
parseStudentData dir = 
  do Y.decodeFile $ dir </> "test/student-test-data.yaml"
