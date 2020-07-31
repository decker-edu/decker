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
import Data.Typeable
import qualified Data.Yaml as Y
import GHC.Generics hiding (Meta)
import Relude
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
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
renderQuestion :: Meta -> Question -> Block
renderQuestion meta qst =
  Div
    ("", ["exa-quest", "columns"], [])
    ([Header 2 nullAttr (parseToInlines (qstTitle qst))] <>
     [Div ("", ["question"], []) $ parseToBlocks (qstQuestion qst)] <>
     renderAnswer (qstAnswer qst) <>
     [ RawBlock
         "html"
         ("<button>" <> lookupInDictionary "exam.solve-button" meta <>
          "</button>")
     ])
  where
    correct (Choice _ True) = "correct"
    correct (Choice _ False) = "wrong"
    renderChoice c =
      [ Div ("", ["choice", correct c], []) $
        Div ("", ["check-box"], []) [] :
        [Div ("", ["content"], []) $ parseToBlocks (choiceTheAnswer c)]
      ]
    renderAnswer (MultipleChoice choices) =
      [Div ("", ["answer", "exa-mc"], []) $ concatMap renderChoice choices]
    renderAnswer (FillText text correct) = undefined
    renderAnswer (FreeForm height answer) =
      [ Div
          ("", ["answer", "exa-ff"], [])
          [ RawBlock
              "html"
              ("<textarea class=\"answer\" placeholder=\"" <>
               lookupInDictionary "exam.placeholder" meta <>
               "\" " <>
               "rows=\"" <>
               show height <>
               "\"" <>
               "></textarea>")
          , Div
              ("", ["solution"], [])
              [ RawBlock "html" $
                "<h3>" <> lookupInDictionary "exam.solution" meta <> "</h3>"
              , Div ("", ["correct"], []) $ parseToBlocks answer
              ]
          ]
      ]
    renderAnswer (MultipleAnswers width answers) = undefined

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
parseToBlocks :: Text -> [Block]
parseToBlocks text =
  case runPure (readMarkdown pandocReaderOpts text) of
    Left err -> throw $ InternalException $ show err
    Right (Pandoc _ blocks) -> blocks

parseToBlock :: Text -> Block
parseToBlock text = do
  case parseToBlocks text of
    [block] -> block
    _ ->
      throw $
      InternalException $
      "cannot parse Markdown to a single block: " <> toString text

parseToInlines :: Text -> [Inline]
parseToInlines text = toInlines $ parseToBlock text

toInlines :: Block -> [Inline]
toInlines (Para inlines) = inlines
toInlines block =
  throw $ InternalException $ "cannot convert block to inlines: " <> show block

examinerFilter :: Pandoc -> Filter Pandoc
examinerFilter pandoc@(Pandoc meta _) = walkM expandQuestion pandoc
  where
    expandQuestion :: Block -> Filter Block
    expandQuestion (Para [Image (id, cls, kvs) _ (url, _)])
      | "question" `elem` cls = do
        uri <- URI.mkURI url
        source <- transformUri uri "" >>= readLocalUri
        let result = Y.decodeEither' $ encodeUtf8 source
        case result of
          Left err -> throw $ InternalException $ show err
          Right question -> return $ renderQuestion meta question
    expandQuestion block = return block
