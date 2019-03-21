module IncludeTests
  ( includeTests
  ) where

import Test.Hspec
import Text.Pandoc.Filter.IncludeCode
import Shake

includeTests = do
  describe "parseInclusionUrl" $ do
    it "does not parse non code: URIs" $
      parseInclusionUrl "/something/else" `shouldBe` Right Nothing
    it "parses code: URIs" $
      parseInclusionUrl "code:/something/codish.md" `shouldBe`
      Right
        (Just
           (InclusionSpec
              { include = "/something/codish.md"
              , mode = EntireFileMode
              , dedent = Nothing
              }))
    it "parses code: URIs with fragments" $
      parseInclusionUrl "code:/something/codish.md#yo" `shouldBe`
      Right
        (Just
           (InclusionSpec
              { include = "/something/codish.md"
              , mode = SnippetMode "yo"
              , dedent = Nothing
              }))
  describe "urlToFilePathIfLocal" $ do
    it "ignores remote urls" $
      urlToFilePathIfLocalIO "." "below" "code:below/file.md" `shouldReturn` "code:below/file.md"
    it "calculates the absolute local path of a releative path" $
      urlToFilePathIfLocalIO "/tmp" "/tmp/below" "file.md" `shouldReturn` "/tmp/below/file.md"
    it "calculates the absolute local path of an absolute path" $
      urlToFilePathIfLocalIO "/tmp" "/tmp/below" "/blip/file.md" `shouldReturn` "/tmp/blip/file.md"
    it "calculates the absolute local path of a releative path" $
      urlToFilePathIfLocalIO "." "/tmp/below" "file.md" `shouldReturn` "/tmp/below/file.md"
    it "calculates the absolute local path of an absolute path" $
      urlToFilePathIfLocalIO "." "/tmp/below" "/blip/file.md" `shouldReturn` "/Users/henrik/workspace/decker/blip/file.md"
