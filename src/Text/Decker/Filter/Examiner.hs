{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings,
  DeriveGeneric, DeriveFunctor, DeriveTraversable #-}

module Text.Decker.Filter.Examiner
  ( Question(..)
  , Answer(..)
  , Choice(..)
  , OneAnswer(..)
  , Difficulty(..)
  , examinerFilter
  ) where

import Control.Exception
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Yaml as Y
import GHC.Generics hiding (Meta)
import Relude
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Paths
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

data Question = Question
  { qstTopicId :: Text
  , qstLectureId :: Text
  , qstTitle :: Text
  , qstPoints :: Int
  , qstQuestion :: Text
  , qstAnswer :: Answer
  , qstDifficulty :: Difficulty
  , qstComment :: Text
  , qstCurrentNumber :: Int
  , qstFilePath :: String
  } deriving (Eq, Show, Typeable, Generic)

data Choice = Choice
  { choiceTheAnswer :: Text
  , choiceCorrect :: Bool
  } deriving (Eq, Show, Typeable)

data OneAnswer = OneAnswer
  { oneDetail :: Text
  , oneCorrect :: Text
  } deriving (Eq, Show, Typeable)

data Answer
  = MultipleChoice { answChoices :: [Choice] }
  | FillText { answFillText :: Text
             , answCorrectWords :: [Text] }
  | FreeForm { answHeightInMm :: Int
             , answCorrectAnswer :: Text }
  | MultipleAnswers { answWidthInMm :: Int
                    , answAnswers :: [OneAnswer] }
  deriving (Eq, Show, Typeable)

data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Eq, Show, Typeable)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Choice)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''OneAnswer)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Answer)

questionOptions = defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON Question where
  toJSON = genericToJSON questionOptions
  toEncoding = genericToEncoding questionOptions

instance FromJSON Question where
  parseJSON (Object q) =
    Question <$> q .: "TopicId" <*> q .: "LectureId" <*> q .: "Title" <*>
    q .: "Points" <*>
    q .: "Question" <*>
    q .: "Answer" <*>
    q .: "Difficulty" <*>
    q .: "Comment" <*>
    q .:? "CurrentNumber" .!= 0 <*>
    q .:? "FilePath" .!= "."
  parseJSON invalid = typeMismatch "Question" invalid

$(deriveJSON defaultOptions ''Difficulty)

readQuestion :: FilePath -> IO Question
readQuestion file = do
  result <- liftIO $ Y.decodeFileEither file
  case result of
    Right question -> return question
    Left exception ->
      throw $
      YamlException $
      "Error parsing question: " ++ file ++ ", " ++ show exception

-- | Renders a question to Pandoc AST.
--
-- The question is enclosed in a DIV that has class `columns` to prevent the
-- HTML writer from sectioning at top-level divs.
renderQuestion :: Meta -> FilePath -> Question -> Block
renderQuestion meta base qst =
  Div
    ("", ["exa-quest", "columns"], [])
    ([Header 2 nullAttr (parseToInlines base (qstTitle qst))] <>
     [Div ("", ["question"], []) $ parseToBlocks base (qstQuestion qst)] <>
     renderAnswer (qstAnswer qst) <>
     [ rawHtml' $
       H.div $ H.button $ toHtml $ lookupInDictionary "exam.solve-button" meta
     ])
  where
    correct (Choice _ True) = "correct"
    correct (Choice _ False) = "wrong"
    renderChoice c =
      [ Div ("", ["choice", correct c], []) $
        Div ("", ["check-box"], []) [] :
        [Div ("", ["content"], []) $ parseToBlocks base (choiceTheAnswer c)]
      ]
    renderAnswer (MultipleChoice choices) =
      [Div ("", ["answer", "exa-mc"], []) $ concatMap renderChoice choices]
    renderAnswer (FillText text correct) =
      throw $ InternalException "FillText questions not yet implemented"
    renderAnswer (FreeForm height answer) =
      [ Div
          ("", ["answer", "exa-ff"], [])
          [ rawHtml' $
            H.textarea ! A.class_ "answer" !
            A.placeholder (toValue $ lookupInDictionary "exam.placeholder" meta) !
            A.rows (show height) $
            ""
          , Div
              ("", ["solution"], [])
              [ rawHtml' $
                H.h3 (toHtml $ lookupInDictionary "exam.solution" meta)
              , Div ("", ["correct"], []) $ parseToBlocks base answer
              ]
          ]
      ]
    -- For now, use OS drop-downs. Later maybe use https://github.com/vorotina/vanilla-select.
    renderAnswer (MultipleAnswers width answers) =
      let select =
            H.select $ H.optgroup $ toHtml $ map mkOption answers
          mkOption (OneAnswer _ correct) = H.option $ toHtml correct
          mkDetail (OneAnswer detail correct) =
            H.tr ! A.class_ "detail" ! dataAttribute "correct" (toValue correct) $
            toHtml [H.td $ toHtml detail, H.td select]
       in rawHtml' $
          H.table ! A.class_ "answer exa-ma" $
          H.tbody $
          toHtml $ map mkDetail $ filter (not . Text.null . oneDetail) answers

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
parseToBlocks :: FilePath -> Text -> [Block]
parseToBlocks base text =
  case adjustResourcePaths base <$> runPure (readMarkdown pandocReaderOpts text) of
    Left err -> throw $ InternalException $ show err
    Right (Pandoc _ blocks) -> blocks

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

examinerFilter :: Pandoc -> Filter Pandoc
examinerFilter pandoc@(Pandoc meta _) = walkM expandQuestion pandoc
  where
    base = lookupMetaOrFail "decker.base-dir" meta
    expandQuestion :: Block -> Filter Block
    expandQuestion (Para [Image (id, cls, kvs) _ (url, _)])
      | "question" `elem` cls = do
        uri <- URI.mkURI url
        source <- transformUri uri "" >>= readLocalUri
        let result = Y.decodeEither' $ encodeUtf8 source
        case result of
          Left err -> throw $ InternalException $ show err
          Right question -> return $ renderQuestion meta base question
    expandQuestion block = return block
