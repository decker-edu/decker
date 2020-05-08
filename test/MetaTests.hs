module MetaTests
  ( metaTests
  ) where

import Text.Decker.Internal.Meta

import qualified Data.Map.Strict as M
import Test.Hspec
import Text.Pandoc hiding (lookupMeta)

m1 =
  Meta
    (M.fromList
       [ ("bool", MetaBool True)
       , ( "write-back"
         , MetaMap
             (M.fromList
                [ ("line-columns", MetaString "80")
                , ("line-wrap", MetaString "none")
                ]))
       , ("top", MetaMap (M.fromList [("bool", MetaBool True)]))
       , ( "list"
         , MetaList
             [ MetaMap (M.fromList [("bool0", MetaBool True)])
             , MetaMap (M.fromList [("bool1", MetaBool True)])
             , MetaMap (M.fromList [("bool2", MetaBool True)])
             ])
       ])

m2 =
  Meta (M.fromList [("level1", MetaMap (M.fromList [("one", MetaString "0")]))])

m2' =
  Meta (M.fromList [("level1", MetaMap (M.fromList [("one", MetaString "1")]))])

m3 = Meta (M.fromList [("list", MetaList [MetaString "some/where/img.png"])])

m3' =
  Meta
    (M.fromList
       [ ( "list"
         , MetaList [MetaString "img.png", MetaString "some/where/img.png"])
       ])

metaTests = do
  describe "getMetaBool" $ do
    it "looks up a top-level boolean meta value" $
      lookupMeta "bool" m1 `shouldBe` Just True
    it "looks up a top-level boolean meta value" $
      lookupMeta "none" m1 `shouldBe` (Nothing :: Maybe Bool)
    it "looks up a boolean meta value" $
      lookupMeta "top.bool" m1 `shouldBe` Just True
    it "looks up a boolean meta value" $
      lookupMeta "top.none" m1 `shouldBe` (Nothing :: Maybe Bool)
    it "looks up a boolean meta value in list" $
      lookupMeta "list[2].bool2" m1 `shouldBe` Just True
  describe "getMetaInt" $
    it "looks up a top-level int meta value" $
    lookupMeta "write-back.line-columns" m1 `shouldBe` Just (80 :: Int)
  describe "getMetaString" $
    it "looks up a top-level int meta value" $
    lookupMeta "write-back.line-wrap" m1 `shouldBe` Just ("none" :: String)
  describe "setMetaValue" $ do
    it "should set the value in a nested map" $
      setMetaValue "level1.one" (MetaString "1") m2 `shouldBe` m2'
    it "should set the value in a nested map" $
      addMetaValue "list" (MetaString "img.png") m3 `shouldBe` m3'
