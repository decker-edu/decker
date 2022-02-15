{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Filter
  ( Question (..),
    Answer (..),
    Choice (..),
    OneAnswer (..),
    Difficulty (..),
    examinerFilter,
  )
where

import Control.Exception
-- import Text.Pretty.Simple

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml as Y
import Relude
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Exam.Question
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Paths
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

renderButton cls text =
  tag "button" $ Div ("", [cls], []) [Plain [Str text]]

-- | Renders a question to Pandoc AST.
renderQuestion :: Attr -> Meta -> FilePath -> Question -> Block
renderQuestion attr meta base qst =
  Div
    ( mergeAttr
        ( "",
          ["exa-quest"],
          [ ("data-points", show $ _qstPoints qst),
            ("data-difficulty", show $ _qstDifficulty qst),
            ("data-topic-id", show $ _qstTopicId qst),
            ("data-lecture-id", show $ _qstLectureId qst)
          ]
        )
        attr
    )
    ( [ Div
          ( "",
            ["difficulty"],
            [ ( "title",
                lookupInDictionary ("exam." <> show (_qstDifficulty qst)) meta
              )
            ]
          )
          []
      ]
        <> [tag "h2" $ Div nullAttr $ parseToBlocks base (_qstTitle qst)]
        <> [Div ("", ["question"], []) $ parseToBlocks base (_qstQuestion qst)]
        <> renderAnswer (_qstAnswer qst)
        <> [renderButton "solve" (lookupInDictionary "exam.solve-button" meta)]
        <> [renderButton "again" (lookupInDictionary "exam.again-button" meta)]
        <> [renderButton "qrcode" (lookupInDictionary "exam.qrcode-button" meta)]
        <> [renderButton "poll" (lookupInDictionary "exam.poll-button" meta)]
        <> [renderButton "stop" (lookupInDictionary "exam.stop-button" meta)]
        <> [ Div
               ("", ["score"], [])
               [ Plain
                   [ Span nullAttr [Str $ lookupInDictionary "exam.points" meta],
                     Span ("", ["display"], []) []
                   ]
               ]
           ]
           -- <> [ rawHtml' $
           --        H.div $ do
           --          H.button ! A.class_ "solve" $ toHtml $ lookupInDictionary "exam.solve-button" meta
           --          H.button ! A.class_ "again" $ toHtml $ lookupInDictionary "exam.again-button" meta
           --          H.div ! A.class_ "score" $ do
           --            H.span $ toHtml (lookupInDictionary "exam.points" meta)
           --            H.span ! A.class_ "display" $ ""
           --    ]
    )
  where
    correct (Choice _ True) = "correct"
    correct (Choice _ False) = "wrong"
    renderChoice c l =
      [ Div ("", ["choice", correct c], []) $
          [ Div ("", ["check-box"], []) [],
            Div ("", ["label"], []) [Plain [Str $ toText [l]]],
            Div ("", ["content"], []) $ parseToBlocks base (_choiceTheAnswer c),
            renderPollChoice c l
          ]
      ]
    renderPollChoice choice label =
      Div
        ("", ["vote"], [("label", toText [label])])
        [ Div ("", ["label"], []) [Plain [Str $ toText [label]]],
          Div ("", ["votes"], []) [Plain [Str "0"]]
        ]
    renderAnswer (MultipleChoice choices) =
      [ Div
          ( "",
            ["answer", "exa-mc"],
            [ ( "data-choices",
                "[" <> Text.intercalate "," (zipWith (\c l -> toText ['"', l, '"']) choices ['A' ..]) <> "]"
              ),
              ("data-votes", show $ length $ filter (_choiceCorrect) choices)
            ]
          )
          (concat $ zipWith renderChoice choices ['A' ..])
      ]
    renderAnswer (FillText text correct) =
      throw $ InternalException "FillText questions not yet implemented"
    renderAnswer (FreeForm height answer) =
      [ Div
          ("", ["answer", "exa-ff"], [])
          [ rawHtml' $
              H.textarea ! A.class_ "answer"
                ! A.placeholder (toValue $ lookupInDictionary "exam.placeholder" meta)
                ! A.rows (show height)
                $ "",
            Div
              ("", ["solution"], [])
              [ rawHtml' $
                  H.h3 (toHtml $ lookupInDictionary "exam.solution" meta),
                Div ("", ["correct"], []) $ parseToBlocks base answer
              ]
          ]
      ]
    renderAnswer (Numerical answer) =
      [ Div
          ("", ["answer", "exa-nu"], [])
          [ rawHtml' $
              H.textarea ! A.class_ "answer"
                ! A.placeholder (toValue $ lookupInDictionary "exam.placeholder" meta)
                ! A.rows "1"
                $ "",
            Div
              ("", ["solution"], [])
              [ rawHtml' $
                  H.h3 (toHtml $ lookupInDictionary "exam.solution" meta),
                Div ("", ["correct"], []) $ parseToBlocks base (show answer)
              ]
          ]
      ]
    -- For now, use OS drop-downs. Later maybe use
    -- https://github.com/vorotina/vanilla-select.
    renderAnswer (MultipleAnswers width answers) =
      let select = H.select $ H.optgroup $ toHtml $ map mkOption answers
          mkOption (OneAnswer _ correct) = H.option $ toHtml correct
          mkDetail (OneAnswer detail correct) =
            H.tr ! A.class_ "detail" ! dataAttribute "correct" (toValue $ correct) $
              toHtml [H.td ! A.class_ "result" $ "", H.td $ toHtml $ parseToBlocks base detail, H.td select]
       in rawHtml' $
            H.table ! A.class_ "answer exa-ma" $
              H.tbody $
                toHtml $ map mkDetail $ filter (not . Text.null . _oneDetail) answers

-- | Left-biased merge of attribute blocks.
mergeAttr :: Attr -> Attr -> Attr
mergeAttr (aid, acls, akv) (bid, bcls, bkv) =
  ( if Text.null aid then bid else aid,
    nub (acls <> bcls),
    Map.toList $ Map.union (Map.fromList akv) (Map.fromList bkv)
  )

{--
toQuiz :: Question -> IO Quiz.Quiz
toQuiz q = do
  title <- parseToInlines (qstTitle q)
  question <- parseToBlocks (qstQuestion q)
  toQuiz' title question (qstAnswer q)
  where
    toQuiz' title question (MultipleChoice choices) = do
      choices' <-
        forM
          choices
          (\choice -> do
             inlines <- parseToInlines (choiceTheAnswer choice)
             return $ Quiz.Choice (choiceCorrect choice) inlines [])
      return $
        Quiz.MultipleChoice
          title
          ["exa-quiz", "exa-mc"]
          Quiz.defaultMeta
          question
          choices'
    toQuiz' title question (MultipleAnswers width answers) =
      throw $ InternalException "Not yet implemented"
    toQuiz' title question (FillText fillText words) =
      throw $ InternalException "Not yet implemented"
    toQuiz' title question (FreeForm height correct) = do
      choice <- parseToBlocks correct
      return $
        Quiz.FreeText
          title
          ["exa-quiz", "exa-ft"]
          Quiz.defaultMeta
          question
          [Quiz.Choice True [] choice]
--}
{--
parseToBlock :: FilePath -> Text -> Block
parseToBlock base text = do
  case parseToBlocks base text of
    [block] -> block
    _ ->
      throw $
        InternalException $
          "cannot parse Markdown to a single block: " <> toString text

parseToInlines :: FilePath -> Text -> [Inline]
parseToInlines base text = toInlines $ parseToBlock base text

toInlines :: Block -> [Inline]
toInlines (Para inlines) = inlines
toInlines block =
  throw $ InternalException $ "cannot convert block to inlines: " <> show block
--}

parseToBlocks :: FilePath -> Text -> [Block]
parseToBlocks base text =
  case adjustResourcePaths base <$> runPure (readMarkdown pandocReaderOpts text) of
    Left err -> throw $ InternalException $ show err
    Right (Pandoc _ blocks) -> blocks

examinerFilter :: Pandoc -> Filter Pandoc
examinerFilter pandoc@(Pandoc meta _) = walkM expandQuestion pandoc
  where
    base = lookupMetaOrFail "decker.base-dir" meta
    expandQuestion :: Block -> Filter Block
    expandQuestion (Para [Image attr@(id, cls, kvs) _ (url, _)])
      | "question" `elem` cls = do
        uri <- URI.mkURI url
        source <- transformUri uri "" >>= readLocalUri
        let result = Y.decodeEither' $ encodeUtf8 source
        case result of
          Left err -> throw $ InternalException $ show err
          Right question -> do
            let q = renderQuestion attr meta base question
            return q
    expandQuestion block = return block
