{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Xml
  ( renderXmlCatalog,
  )
where

import Relude
import Text.Decker.Internal.URI
import Text.Decker.Internal.Exception
import Text.Decker.Exam.Question
import Text.Decker.Exam.Render hiding (renderQuestion)
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.IORef
import Data.List
import Data.List.Extra
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import Data.Typeable
import Data.XML.Types (Content (..))
import qualified Data.Yaml as Y
import Data.Yaml.Pretty as Y
import Debug.Trace
import Development.Shake
import Development.Shake.FilePath
import Network.URI
import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob as Glob
import System.Process
import System.Random
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF
import Text.Pandoc.Walk
import Text.Printf
import qualified Text.XML as XML

-- Renders a catalog of all questions sorted by LectureId and TopicId.
renderXmlCatalog ::
  FilePath -> [Question] -> FilePath -> Action ()
renderXmlCatalog projectDir questions out = do
  rendered <- mapM (renderQuestion . insertTitle) questions
  let sorted = sortQuestions rendered
      nodes = concatMap renderXML sorted
      quiz = XML.Element "quiz" M.empty nodes
      document = XML.Document (XML.Prologue [] Nothing []) quiz []
  liftIO $ XML.writeFile def out document
  where
    renderXML :: Question -> [XML.Node]
    renderXML question =
      [renderCategoryXML question, renderQuestionXML question]
    renderCategoryXML :: Question -> XML.Node
    renderCategoryXML question =
      XML.NodeElement $
        XML.Element
          "question"
          (M.fromList [("type", "category")])
          [ XML.NodeElement $
              XML.Element
                "category"
                M.empty
                [ XML.NodeElement $
                    XML.Element
                      "text"
                      M.empty
                      [ XML.NodeContent $
                          T.concat
                            [ "$course$/",
                              _qstLectureId question,
                              "/",
                              _qstTopicId question
                            ]
                      ]
                ],
            XML.NodeElement $
              XML.Element
                "questiontext"
                (M.fromList [("format", "html")])
                [ XML.NodeElement $
                    XML.Element
                      "text"
                      M.empty
                      [XML.NodeContent (_qstQuestion question)]
                ]
          ]
    renderQuestionXML :: Question -> XML.Node
    renderQuestionXML question =
      XML.NodeElement $
        XML.Element
          "question"
          (M.fromList [("type", xmlAnswerType question)])
          ( [ XML.NodeElement $
                XML.Element
                  "name"
                  M.empty
                  [ XML.NodeElement $
                      XML.Element "text" M.empty [XML.NodeContent (_qstTitle question)]
                  ],
              XML.NodeElement $
                XML.Element
                  "questiontext"
                  (M.fromList [("format", "html")])
                  [ XML.NodeElement $
                      XML.Element
                        "text"
                        M.empty
                        [XML.NodeContent (_qstQuestion question)]
                  ],
              XML.NodeElement $
                XML.Element "answernumbering" M.empty [XML.NodeContent "none"],
              XML.NodeElement $
                XML.Element "shuffleanswers" M.empty [XML.NodeContent "1"],
              XML.NodeElement $
                XML.Element
                  "defaultgrade"
                  M.empty
                  [XML.NodeContent (T.pack $ show (_qstPoints question))],
              XML.NodeElement $ XML.Element "penalty" M.empty [XML.NodeContent "0"],
              XML.NodeElement $
                XML.Element
                  "single"
                  M.empty
                  [ XML.NodeContent
                      ( if hasSingleAnswer question
                          then "1"
                          else "0"
                      )
                  ]
            ]
              ++ renderAnswer (_qstAnswer question)
          )
      where
        answer = renderAnswer (_qstAnswer question)
        renderAnswer (MultipleChoice choices) =
          map (renderChoice $ correctAnswers choices) choices
        renderAnswer (FillText fillText correctWords) =
          [ XML.NodeElement $
              XML.Element
                "answer"
                (M.fromList [("fraction", "0")])
                [XML.NodeElement $ XML.Element "text" M.empty []]
          ]
        renderAnswer (FreeForm heightInMm correctAnswer) =
          [ XML.NodeElement $
              XML.Element
                "answer"
                (M.fromList [("fraction", "0")])
                [XML.NodeElement $ XML.Element "text" M.empty []]
          ]
        renderAnswer (MultipleAnswers widthInMm answers) =
          map renderSubQuestion answers
        renderSubQuestion answer =
          XML.NodeElement $
            XML.Element
              "subquestion"
              M.empty
              [ XML.NodeElement $
                  XML.Element "text" M.empty [XML.NodeContent (_oneDetail answer)],
                XML.NodeElement $
                  XML.Element
                    "answer"
                    M.empty
                    [ XML.NodeElement $
                        XML.Element
                          "text"
                          M.empty
                          [XML.NodeContent (_oneCorrect answer)]
                    ]
              ]
        renderChoice n (Choice answer correct) =
          XML.NodeElement $
            XML.Element
              "answer"
              ( M.fromList
                  [ ( "fraction",
                      if correct
                        then T.pack $ show (100.0 / fromIntegral n)
                        else
                          T.pack $
                            show
                              ( if n > 1
                                  then -100.0
                                  else 0
                              )
                    )
                  ]
              )
              [ XML.NodeElement $
                  XML.Element "text" M.empty [XML.NodeContent answer]
              ]

correctAnswers :: [Choice] -> Int
correctAnswers choices = length (filter _choiceCorrect choices)

hasSingleAnswer :: Question -> Bool
hasSingleAnswer question =
  case _qstAnswer question of
    MultipleChoice choices -> 1 == correctAnswers choices
    _ -> False

xmlAnswerType :: Question -> T.Text
xmlAnswerType q =
  case _qstAnswer q of
    MultipleChoice choices -> "multichoice"
    FillText fillText correctWords -> "essay"
    FreeForm heightInMm correctAnswer -> "essay"
    MultipleAnswers widthInMm answers -> "matching"


-- | Inserts the title into the question text.
insertTitle :: Question -> Question
insertTitle q =
  q {_qstQuestion = T.concat ["# ", _qstTitle q, "\n\n", _qstQuestion q]}

sortQuestions :: [Question] -> [Question]
sortQuestions =
  sortBy
    (\a b ->
       case compare (_qstLectureId a) (_qstLectureId b) of
         EQ -> compare (_qstTopicId a) (_qstTopicId b)
         c -> c)

renderQuestion :: Question -> Action Question
renderQuestion question =
  mapM (renderHtml (takeDirectory $ _qstFilePath question)) question

embedImages :: FilePath -> Inline -> Action Inline
embedImages base (Image (id, cls, kv) inlines (url, title)) = do
  let urlStr = toString url
  imgData <- liftIO $ readFileBase64 (makeProjectPath base urlStr)
  let ext = takeExtension urlStr
  let dataUrl = concat ["data:image/", drop 1 ext, ";base64,", imgData]
  let styled =
        ("style", "width:100%;max-width:40em;") : filter ((/=) "style" . fst) kv
  return $ Image (id, cls, styled) inlines (toText dataUrl, title)
embedImages base inline = return inline

embedCode :: Block -> Block
embedCode (CodeBlock attr code) = CodeBlock attr code
embedCode block = block

readFileBase64 :: FilePath -> IO String
readFileBase64 path = UTF8.toString . B64.encode <$> BS.readFile path

renderHtml :: FilePath -> T.Text -> Action T.Text
renderHtml base markdown =
  case parseMarkdown markdown of
    Right pandoc -> do
      embedded <- walkM (embedImages base) (walk embedCode pandoc)
      case renderHtml5 embedded of
        Right html5 -> return html5
        Left errMsg -> throw $ PandocException (show errMsg)
    Left errMsg -> throw $ PandocException (show errMsg)
  where
    readerOptions = def {readerExtensions = pandocExtensions}
    writerOptions =
      def
        { writerHTMLMathMethod = MathJax ""
        , writerExtensions = pandocExtensions
        , writerHighlightStyle = Nothing
        }
    parseMarkdown = runPure . readMarkdown readerOptions
    renderHtml5 = runPure . writeHtml5String writerOptions

useCDATA :: Content -> Bool
useCDATA (ContentText txt) = False -- length (T.lines txt) > 1
useCDATA _ = False

