{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Xml
  ( renderXmlCatalog,
  )
where

import Control.Exception
import Control.Lens hiding (Choice)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Relude
import Text.Decker.Exam.Question
import Text.Decker.Internal.Exception
import Text.Decker.Internal.URI
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.XML as XML

-- Renders a catalog of all questions sorted by LectureId and TopicId.
renderXmlCatalog ::
  [Question] -> FilePath -> Action ()
renderXmlCatalog questions out = do
  rendered <- mapM (renderMarkdownFields . insertTitle) questions
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
                XML.Element
                  "shuffleanswers"
                  M.empty
                  [XML.NodeContent (if _qstShuffleAnswers question then "1" else "0")],
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
                (M.fromList [("fraction", "100")])
                [XML.NodeElement $ XML.Element "text" M.empty [XML.NodeContent correctAnswer]]
          ]
        renderAnswer (Numerical correctAnswer) =
          [ XML.NodeElement $
              XML.Element
                "answer"
                (M.fromList [("fraction", "100")])
                [XML.NodeElement $ XML.Element "text" M.empty [XML.NodeContent (show correctAnswer)]]
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
    Numerical correctAnswer -> "numerical"
    MultipleAnswers widthInMm answers -> "matching"

-- | Inserts the title into the question text.
insertTitle :: Question -> Question
insertTitle q =
  let style =
        "<style>"
          <> " div.formulation.clearfix { background-color: #f9f9f9; color: #000; border: solid 1px #ccc; }"
          <> " div.formulation.clearfix h1 { font-size: 1.5rem; font-weight: bold; }"
          <> "</style>\n\n"
   in q {_qstQuestion = T.concat [style, "# ", _qstTitle q, "\n\n", _qstQuestion q]}

sortQuestions :: [Question] -> [Question]
sortQuestions =
  sortBy
    ( \a b ->
        case compare (_qstLectureId a) (_qstLectureId b) of
          EQ -> compare (_qstTopicId a) (_qstTopicId b)
          c -> c
    )

renderMarkdownFields :: Question -> Action Question
renderMarkdownFields question = do
  traverseOf qstTitle render question
    >>= traverseOf qstQuestion render
    >>= traverseOf qstAnswer renderAnswer
  where
    render = renderHtml (takeDirectory $ _qstFilePath question)
    renderAnswer answer@MultipleChoice {} =
      traverseOf (answChoices . each . choiceTheAnswer) render answer
    renderAnswer answer@FillText {} = traverseOf answFillText render answer
    renderAnswer answer@FreeForm {} = return answer
    renderAnswer answer@Numerical {} = return answer
    renderAnswer answer@MultipleAnswers {} =
      traverseOf (answAnswers . each . oneDetail) render answer
        >>= traverseOf (answAnswers . each . oneCorrect) render

embedImages :: FilePath -> Inline -> Action Inline
embedImages base (Image (id, cls, kv) inlines (url, title)) = do
  let urlStr = toString url
  imgData <- liftIO $ readFileBase64 (makeProjectPath base urlStr)
  let ext = takeExtension urlStr
  let dataUrl = concat ["data:image/", drop 1 ext, ";base64,", imgData]
  -- let styled =
  --       ("style", "width:100%;max-width:40em;") : filter ((/=) "style" . fst) kv
  return $ Image (id, cls, kv) inlines (toText dataUrl, title)
embedImages base inline = return inline

embedCode :: Block -> Block
embedCode (CodeBlock attr code) = CodeBlock attr code
embedCode block = block

killSinglePara :: Pandoc -> Pandoc
killSinglePara (Pandoc meta [Para inlines]) = Pandoc meta [Plain inlines]
killSinglePara pandoc = pandoc

readFileBase64 :: FilePath -> IO String
readFileBase64 path = toString . B64.encodeBase64 <$> BS.readFile path

renderHtml :: FilePath -> T.Text -> Action T.Text
renderHtml base markdown =
  case parseMarkdown markdown of
    Right pandoc -> do
      embedded <- killSinglePara <$> walkM (embedImages base) (walk embedCode pandoc)
      case renderHtml5 embedded of
        Right html5 -> return html5
        Left errMsg -> throw $ PandocException (show errMsg)
    Left errMsg -> throw $ PandocException (show errMsg)
  where
    readerOptions = def {readerExtensions = pandocExtensions}
    writerOptions =
      def
        { writerHTMLMathMethod = MathJax "",
          writerExtensions = pandocExtensions,
          writerHighlightStyle = Nothing
        }
    parseMarkdown = runPure . readMarkdown readerOptions
    renderHtml5 = runPure . writeHtml5String writerOptions

-- useCDATA :: Content -> Bool
-- useCDATA (ContentText txt) = False -- length (T.lines txt) > 1
-- useCDATA _ = False
