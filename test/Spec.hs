{-# LANGUAGE OverloadedStrings #-}

-- Import WatchTests
--  import ShortLinkTests
import Control.Lens ( (^.) )

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as Text

import IncludeTests

import MediaTests

import MetaTests

import System.FilePath

import Test.Hspec

import Text.Decker.Internal.URI
import Text.Decker.Project.Project as P
import qualified Text.URI as URI

-- mediaTests
-- includeTests
-- shortLinkTests
-- watchTests
-- sketchTests
-- metaTests
main =
  do dirs <- projectDirectories
     --
     deckTemplate <- B.readFile
       (dirs ^. project </> "resource/template/deck.html")
     --
     hspec $
       do describe "makeRelativeTo" $
            it
              "calculates the path of file relative to dir. Includes '..'" $
            do makeRelativeTo "" "img.png" `shouldBe` "img.png"
               makeRelativeTo "/one/two" "/one/two/img.png" `shouldBe`
                 "img.png"
               makeRelativeTo
                 "/one/two/three"
                 "/one/two/four/img.png" `shouldBe`
                 joinPath [ "..", "four", "img.png" ]
               makeRelativeTo
                 "/some/where/else"
                 "/one/two/four/img.png" `shouldBe`
                 joinPath
                   [ ".."
                   , ".."
                   , ".."
                   , "one"
                   , "two"
                   , "four"
                   , "img.png"
                   ]
               makeRelativeTo
                 "/Users/henrik/tmp/decker-demo/public"
                 "/Users/henrik/tmp/decker-demo/public/cache/b48cadafb942dc1426316772321dd0c7.png" `shouldBe`
                 joinPath
                   [ "cache", "b48cadafb942dc1426316772321dd0c7.png" ]
          --
          describe "removeCommonPrefix" $
            it "removes the common prefix from two pathes." $
            do P.removeCommonPrefix ( "", "" ) `shouldBe` ( "", "" )
               P.removeCommonPrefix
                 ( "fasel/bla", "fasel/bla/lall" ) `shouldBe`
                 ( "", "lall" )
               P.removeCommonPrefix
                 ( "lurgel/hopp", "fasel/bla/lall" ) `shouldBe`
                 ( joinPath [ "lurgel", "hopp" ]
                 , joinPath [ "fasel", "bla", "lall" ]
                 )
               P.removeCommonPrefix
                 ( "/lurgel/hopp", "fasel/bla/lall" ) `shouldBe`
                 ( joinPath [ "/lurgel", "hopp" ]
                 , joinPath [ "fasel", "bla", "lall" ]
                 )
               P.removeCommonPrefix
                 ( "/lurgel/hopp", "/fasel/bla/lall" ) `shouldBe`
                 ( joinPath [ "lurgel", "hopp" ]
                 , joinPath [ "fasel", "bla", "lall" ]
                 )
          describe "makeAbsolutePath" $
            it "creates an absolute path" $
            do makeAbsolutePath "/project" "/project/base" "/this" `shouldBe`
                 "/project/this"
               makeAbsolutePath "/project" "/project/base" "this" `shouldBe`
                 "/project/base/this"
          describe "makeAbsolutePathIfLocal a" $
            it "ignores non existent files" $
            do makeAbsolutePathIfLocal
                 "/project"
                 "/project/base"
                 "/this" `shouldReturn`
                 "/this"
          describe "makeAbsolutePathIfLocal b" $
            it "creates an absolute path" $
            do makeAbsolutePathIfLocal
                 (dirs ^. project)
                 ((dirs ^. project) </> "test/decks")
                 "include/something.md" `shouldReturn`
                 (Text.pack
                    ((dirs ^. project) </>
                     "test/decks/include/something.md"))
               makeAbsolutePathIfLocal
                 (dirs ^. project)
                 ((dirs ^. project) </> "test/decks")
                 "/test/decks/include/06-metal.png" `shouldReturn`
                 (Text.pack
                    ((dirs ^. project) </>
                     "test/decks/include/06-metal.png"))
          describe "isUriPathLocal" $
            it "knows local URIs" $
            do isUriPathLocal <$> URI.mkURI "" `shouldReturn` False
               isUriPathLocal <$>
                 URI.mkURI "local.file" `shouldReturn` True
               isUriPathLocal <$>
                 URI.mkURI "/local.file" `shouldReturn` True
               isUriPathLocal <$>
                 URI.mkURI "file:local.file" `shouldReturn` True
               isUriPathLocal <$>
                 URI.mkURI
                   "file:/local.file?lork=bla#frag" `shouldReturn`
                 True
               isUriPathLocal <$>
                 URI.mkURI "file:/local.file" `shouldReturn` True
               isUriPathLocal <$>
                 URI.mkURI "http:///local.file" `shouldReturn` False
          describe "absoluteUriPathIfLocal" $
            it
              "converts a local URI with relative path to absolute path" $
            do (do relUri <- URI.mkURI "include/06-metal.png"
                   absUri <- URI.mkURI $
                     Text.pack
                       (((dirs ^. project) </> "test/decks") </>
                        "include/06-metal.png")
                   absoluteUriPathIfLocal
                     (dirs ^. project)
                     (dirs ^. project </> "test/decks")
                     relUri `shouldReturn`
                     Just absUri)
               (do relUri <-
                     URI.mkURI "/test/decks/include/06-metal.png"
                   absUri <- URI.mkURI $
                     Text.pack
                       (((dirs ^. project) </> "test/decks") </>
                        "include/06-metal.png")
                   absoluteUriPathIfLocal
                     (dirs ^. project)
                     (dirs ^. project </> "test/decks")
                     relUri `shouldReturn`
                     Just absUri)
          describe "absoluteUriPathIfLocal" $
            it "and keeps the bling" $
            do relUri <- URI.mkURI "include/06-metal.png#fragfrag"
               absUri <- URI.mkURI $
                 Text.pack
                   ((dirs ^. project) </>
                    "test/decks/include/06-metal.png#fragfrag")
               absoluteUriPathIfLocal
                 (dirs ^. project)
                 (dirs ^. project </> "test/decks")
                 relUri `shouldReturn`
                 Just absUri
