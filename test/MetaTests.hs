module MetaTests
  ( metaTests
  ) where

import qualified Data.Map.Strict as M
import Text.Decker.Internal.Meta
import Test.Hspec
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
  describe "lookupMetaBool" $ do
    it "looks up a top-level boolean meta value" $
      lookupMetaBool m1 "bool" `shouldBe` Just True
    it "looks up a top-level boolean meta value" $
      lookupMetaBool m1 "none" `shouldBe` Nothing
    it "looks up a boolean meta value" $
      lookupMetaBool m1 "top.bool" `shouldBe` Just True
    it "looks up a boolean meta value" $
      lookupMetaBool m1 "top.none" `shouldBe` Nothing
    it "looks up a boolean meta value in list" $
      lookupMetaBool m1 "list[2].bool2" `shouldBe` Just True
  describe "lookupMetaInt" $
    it "looks up a top-level int meta value" $
    lookupMetaInt m1 "write-back.line-columns" `shouldBe` Just 80
  describe "lookupMetaString" $
    it "looks up a top-level int meta value" $
    lookupMetaString m1 "write-back.line-wrap" `shouldBe` Just "none"
