{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Maybe
import Data.Text
import Text.Pandoc
import Utilities
import System.FilePath
import System.FilePath.Posix
import System.FilePath.Glob
import qualified Data.Yaml as Y
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

main =
  do projectDir <- calcProjectDirectory
     --
     let publicDir = projectDir </> "public"
     let cacheDir = publicDir </> "cache"
     let supportDir = publicDir </> "support"
     --
     metaFiles <- globDir1 (compile "**/*-meta.yaml") projectDir
     putStrLn $ show metaFiles
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
