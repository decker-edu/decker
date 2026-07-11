{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Render
  ( renderQuestion,
    renderCatalog,
    renderQuestionCatalogJson,
  )
where

import Control.Exception
import Control.Lens hiding (Choice, (.=))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.IO qualified as Text
import Development.Shake hiding (Resource)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
-- import Text.Groom

import Relude
import Relude.Extra.Group
import System.FilePath.Posix
import Text.Blaze qualified as A
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Decker.Exam.Question
import Text.Decker.Exam.Xml (renderMarkdownFields)
import Text.Decker.Filter.Decker2 (deckerMediaFilter)
import Text.Decker.Filter.Paths
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Internal.MetaExtra (mergeDocumentMeta)
import Text.Decker.Writer.Layout
import Text.Pandoc
import Text.Pandoc.Walk

-- import Text.Pretty.Simple

compileQuestionToHtml :: Meta -> FilePath -> Question -> Action Question
compileQuestionToHtml meta _base quest = do
  -- The question file path is absolute; the media filter needs a
  -- project-relative base so referenced resources are provisioned into public/
  -- (publicDir </> absolutePath would collapse to the absolute source path and
  -- nothing would be copied).
  cwd <- liftIO getCurrentDirectory
  let base = makeRelative cwd (dropFileName (quest ^. qstFilePath))
      render = renderSnippetToHtml meta base
      compileAnswerToHtml :: Answer -> Action Answer
      compileAnswerToHtml mc@MultipleChoice {} =
        traverseOf (answChoices . traverse . choiceTheAnswer) render mc
      compileAnswerToHtml ma@MultipleAnswers {} =
        traverseOf (answAnswers . traverse . oneDetail) render
          =<< traverseOf (answAnswers . traverse . oneCorrect) render ma
      compileAnswerToHtml ff@FreeForm {} =
        traverseOf answCorrectAnswer render ff
      compileAnswerToHtml nu@Numerical {} = return nu
      compileAnswerToHtml ft@FillText {} =
        traverseOf (answCorrectWords . traverse) render ft
  traverseOf qstTitle render
    =<< traverseOf qstQuestion render
    =<< traverseOf qstAnswer compileAnswerToHtml quest

-- | Renders a Markdown snippet to HTML applying the full Decker media filter.
renderSnippetToHtml :: Meta -> FilePath -> Text -> Action Text
renderSnippetToHtml meta base markdown = do
  pandoc <- liftIO $ handleError $ runPure $ readMarkdown pandocReaderOpts markdown
  let options = pandocWriterOpts {writerHTMLMathMethod = MathJax "Handled in the render"}
  filtered <-
    mergeDocumentMeta (setMetaValue "decker.use-data-src" False meta) pandoc
      >>= adjustResourcePathsA base
      -- >>= (\p -> print p >> return p)
      >>= deckerMediaFilter (Disposition Page Html) (base </> "dummy.md")
  liftIO $ handleError $ runPure $ writeHtml45String options meta $ walk dropPara filtered

-- | Drops a leading Para block wrapper for a Plain wrapper.
dropPara (Para inlines) = Plain inlines
dropPara block = block

renderAnswerToHtml :: Answer -> Html
renderAnswerToHtml answer@MultipleChoice {} =
  H.ul ! A.class_ "answer multiple-choice" $ toHtml $ map render $ answer ^. answChoices
  where
    render choice =
      let solution = if choice ^. choiceCorrect then "correct" else "wrong"
       in H.li ! A.class_ solution $ do
            H.span $ if choice ^. choiceCorrect then "☑" else "☐"
            H.span $ preEscapedText $ choice ^. choiceTheAnswer
renderAnswerToHtml answer@MultipleAnswers {} =
  H.table ! A.class_ "answer multiple-answers" $ toHtml $ map render $ answer ^. answAnswers
  where
    render one =
      H.tr $ do
        H.th (preEscapedText $ one ^. oneDetail)
        H.td (preEscapedText $ one ^. oneCorrect)
renderAnswerToHtml answer@FreeForm {} = do
  preEscapedText $ answer ^. answCorrectAnswer
renderAnswerToHtml answer@Numerical {} = do
  H.text $ answer ^. answCorrectAnswer
renderAnswerToHtml answer@FillText {} =
  H.p "Not yet implemented"

hn :: Int -> Html -> Html
hn 1 = H.h1
hn 2 = H.h2
hn 3 = H.h3
hn 4 = H.h4
hn 5 = H.h5
hn 6 = H.h6
hn n = throw $ InternalException $ "Haha, good one: H" <> show n

renderQuestionToHtml :: Int -> Text -> Meta -> Question -> Html
renderQuestionToHtml h id meta quest = do
  let editor :: String = lookupMetaOrElse "zed://file" "editor.link-prefix" meta
  H.div
    ! A.class_ "question"
    ! A.id (toValue id)
    $ do
      hn h $ do
        preEscapedText $ quest ^. qstTitle
        H.small
          $ H.a
          ! A.class_ "editor-link"
          ! A.href (toValue (editor <> toString (quest ^. qstFilePath)))
          $ "(Edit)"
      H.div ! A.class_ "closed" $ do
        H.p $ preEscapedText $ quest ^. qstQuestion
        hn (h + 1) "Answer"
        H.p $ renderAnswerToHtml $ quest ^. qstAnswer
        H.div ! A.class_ "question-meta" $ do
          metaItem "Lecture" $ H.code $ toHtml (quest ^. qstLectureId)
          metaItem "Topic" $ H.code $ toHtml (quest ^. qstTopicId)
          metaItem "Exam" $ toHtml (if quest ^. qstExam then "yes" else "no" :: Text)
          H.a
            ! A.class_ "editor-link open-in-editor"
            ! A.href (toValue (editor <> toString (quest ^. qstFilePath)))
            $ "Open in Zed"
  where
    metaItem :: Text -> Html -> Html
    metaItem key value =
      H.span ! A.class_ "meta-item" $ do
        H.span ! A.class_ "meta-key" $ toHtml key
        H.span ! A.class_ "meta-val" $ value

renderQuestionDocument :: Meta -> FilePath -> Question -> Action Text
renderQuestionDocument meta base quest = do
  -- htmlQuest <- compileQuestionToHtml meta base quest
  htmlQuest <- renderMarkdownFields quest
  let html = renderQuestionToHtml 2 "" meta htmlQuest
  return
    $ toText
    $ renderHtml
    $ H.html
    $ do
      H.head $ do
        H.meta ! A.charset "utf-8"
        H.script ! A.src "/support/vendor/mathjax/tex-svg.js" $ ""
        H.script ! A.src "/support/js/quest.js" $ ""
        H.link ! A.rel "stylesheet" ! A.href "/support/css/quest.css"
        H.title (preEscapedText $ quest ^. qstTitle)
      H.body html

renderQuestionBrowser :: FilePath -> [Question] -> Action Text
renderQuestionBrowser base questions = do
  return
    $ toText
    $ renderHtml
    $ H.html
    $ do
      H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "Question Catalog"
        H.script ! A.type_ "module" ! A.src "/support/js/catalog.js" $ ""
        H.script ! A.src "/support/vendor/mathjax/tex-svg.js" $ ""
        H.script ! A.src "/support/js/reload.js" $ ""
        H.link ! A.rel "stylesheet" ! A.href "/support/css/catalog.css"
      H.body $ do
        H.header $ do
          H.h1 ("Question Browser (" <> show (length questions) <> ")")
          H.div ! A.class_ "panel" $ do
            H.div ! A.class_ "lectures" $ lectureIds
            H.div ! A.class_ "topics" $ topicIds
            H.div ! A.class_ "questions" $ topicQuests
            H.iframe ! A.class_ "questions" ! A.src "" $ ""
  where
    grouped = groupQuestions questions
    lectureIds = toHtml $ map (lectureButton . fst) grouped
    topicIds = toHtml $ map lectureTopics grouped
    lectureButton lid =
      H.button
        ! A.type_ "radio"
        ! A.name "lecture"
        ! A.dataAttribute "lecture" (toValue lid)
        $ toHtml lid
    topicButton tid =
      H.button
        ! A.type_ "radio"
        ! A.name "topic"
        ! A.dataAttribute "topic" (toValue tid)
        $ toHtml tid
    questButton quest =
      H.button
        ! A.type_ "radio"
        ! A.name "question"
        ! A.dataAttribute "src" (toValue $ quest ^. qstTitle)
        $ toHtml
        $ quest
        ^. qstTitle
    topicQuests =
      toHtml
        $ concatMap
          ( \(lid, topics) ->
              map (questTitles lid) topics
          )
          grouped
    lectureTopics (lid, topics) =
      H.div
        ! A.dataAttribute "lecture" (toValue lid)
        $ toHtml
        $ map (topicButton . fst) topics
    questTitles lid (tid, quests) =
      H.div
        ! A.dataAttribute "lecture" (toValue lid)
        ! A.dataAttribute "topic" (toValue tid)
        $ toHtml
        $ map questButton quests

groupQuestions :: [Question] -> [(Text, [(Text, [Question])])]
groupQuestions questions = sorted
  where
    grouped :: HashMap Text (HashMap Text (NonEmpty Question))
    grouped = HashMap.map (groupBy _qstTopicId) (groupBy _qstLectureId questions)
    sorted :: [(Text, [(Text, [Question])])]
    sorted =
      List.sortOn fst
        $ map
          ( \(k, v) ->
              ( k,
                List.sortOn fst
                  $ map
                    ( \(k, v) ->
                        (k, List.sortOn _qstTitle $ NonEmpty.toList v)
                    )
                  $ HashMap.toList v
              )
          )
        $ HashMap.toList grouped

instance (ToMarkup a) => ToMarkup (NonEmpty a) where
  toMarkup = toHtml . map toMarkup . toList

renderQuestion :: Meta -> FilePath -> FilePath -> Action ()
renderQuestion meta src out =
  do
    let base = takeDirectory src
    putInfo $ "# render ('" <> src <> "' for '" <> out <> "' with base '" <> base <> "')"
    liftIO (readQuestion src)
      >>= renderQuestionDocument meta base
      >>= (liftIO . Text.writeFile out)

renderCatalog :: Meta -> [FilePath] -> FilePath -> Action ()
renderCatalog meta files out =
  do
    let base = takeDirectory out
    putInfo $ "# catalog (for " <> out <> ")"
    questions <- liftIO $ mapM readQuestion files
    mapM (compileQuestionToHtml meta base) questions
      >>= renderQuestionBrowser base
      >>= (liftIO . Text.writeFile out)

-- | A flattened, fully rendered view of a single question for consumption by
-- the exam-builder web app. The question markdown fields are compiled to HTML
-- here (server-side) so the browser only needs to display them.
data QuestionView = QuestionView
  { qvLectureId :: Text,
    qvTopicId :: Text,
    qvTitle :: Text,
    qvPoints :: Int,
    qvDifficulty :: Text,
    qvExam :: Bool,
    qvFilePath :: Text,
    -- | The question's directory relative to the project root. Image and other
    -- resource URLs in the rendered HTML are relative to this directory; the
    -- web app uses it to resolve them against the server root (public/).
    qvBase :: Text,
    qvHtml :: Text
  }

instance ToJSON QuestionView where
  toJSON v =
    object
      [ "lectureId" .= qvLectureId v,
        "topicId" .= qvTopicId v,
        "title" .= qvTitle v,
        "points" .= qvPoints v,
        "difficulty" .= qvDifficulty v,
        "exam" .= qvExam v,
        "filePath" .= qvFilePath v,
        "base" .= qvBase v,
        "html" .= qvHtml v
      ]

-- | Pre-renders all questions to a JSON catalog for the exam-builder web app.
-- Each question's markdown fields are compiled to HTML via 'compileQuestionToHtml'
-- and the whole question is rendered to an HTML preview snippet.
renderQuestionCatalogJson :: Meta -> [FilePath] -> FilePath -> Action ()
renderQuestionCatalogJson meta files out = do
  putInfo $ "# exam-builder catalog (for " <> out <> ")"
  cwd <- liftIO getCurrentDirectory
  questions <- liftIO $ mapM readQuestion files
  views <- mapM (toView cwd) questions
  liftIO $ createDirectoryIfMissing True (takeDirectory out)
  liftIO $ LBS.writeFile out (Aeson.encode views)
  where
    toView cwd quest = do
      let base = dropFileName (quest ^. qstFilePath)
          -- Project-relative directory; resource URLs in the HTML are relative
          -- to this and must be resolved against public/ by the web app.
          relBase = dropTrailingPathSeparator (makeRelative cwd base)
      compiled <- compileQuestionToHtml meta base quest
      let html = toText $ renderHtml $ renderQuestionToHtml 2 "" meta compiled
      return
        QuestionView
          { qvLectureId = quest ^. qstLectureId,
            qvTopicId = quest ^. qstTopicId,
            qvTitle = quest ^. qstTitle,
            qvPoints = quest ^. qstPoints,
            qvDifficulty = show (quest ^. qstDifficulty),
            qvExam = quest ^. qstExam,
            qvFilePath = toText (quest ^. qstFilePath),
            qvBase = toText relBase,
            qvHtml = html
          }
