module MetaTests
  ( metaTests
  ) where

import qualified Data.Map.Strict as M
import Test.Hspec
import Text.Decker.Internal.Meta
import Text.Pandoc

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

metaTests = do
  describe "getMetaBool" $ do
    it "looks up a top-level boolean meta value" $
      getMetaBool "bool" m1 `shouldBe` Just True
    it "looks up a top-level boolean meta value" $
      getMetaBool "none" m1 `shouldBe` Nothing
    it "looks up a boolean meta value" $
      getMetaBool "top.bool" m1 `shouldBe` Just True
    it "looks up a boolean meta value" $
      getMetaBool "top.none" m1 `shouldBe` Nothing
    it "looks up a boolean meta value in list" $
      getMetaBool "list[2].bool2" m1 `shouldBe` Just True
  describe "getMetaInt" $
    it "looks up a top-level int meta value" $
    getMetaInt "write-back.line-columns" m1 `shouldBe` Just 80
  describe "getMetaString" $
    it "looks up a top-level int meta value" $
    getMetaString "write-back.line-wrap" m1 `shouldBe` Just "none"
