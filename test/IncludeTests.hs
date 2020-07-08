{-# LANGUAGE OverloadedStrings #-}

module IncludeTests ( includeTests ) where

import Test.Hspec

import Text.Decker.Filter.IncludeCode

includeTests =
    do describe "parseInclusionUrl" $
           do it "does not parse non code: URIs" $
                  parseInclusionUrl "/something/else" `shouldBe`
                  Right Nothing
              it "parses code: URIs" $
                  parseInclusionUrl
                      "code:/something/codish.md" `shouldBe`
                  Right
                      (Just
                           (InclusionSpec
                            { include = "/something/codish.md"
                            , mode = EntireFileMode
                            , dedent = Nothing
                            }))
              it "parses code: URIs with fragments" $
                  parseInclusionUrl
                      "code:/something/codish.md#yo" `shouldBe`
                  Right
                      (Just
                           (InclusionSpec
                            { include = "/something/codish.md"
                            , mode = SnippetMode "yo"
                            , dedent = Nothing
                            }))
