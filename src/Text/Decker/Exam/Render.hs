{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Render
  ( renderQuestion,
    renderCatalog,
  )
where

import Control.Exception
import Control.Lens hiding (Choice)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as Text
import Development.Shake hiding (Resource)
-- import Text.Groom

import Relude
import Relude.Extra.Group
import System.FilePath.Posix
import qualified Text.Blaze as A
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Exam.Question
import Text.Decker.Filter.Paths
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Reader.Markdown
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Decker.Writer.Layout

-- import Text.Pretty.Simple

compileQuestionToHtml :: Meta -> FilePath -> Question -> Action Question
compileQuestionToHtml meta base quest = do
 traverseOf qstTitle render
    =<< traverseOf qstQuestion render
    =<< traverseOf qstAnswer (compileAnswerToHtml meta base) quest
  where
    render = renderSnippetToHtml meta base
    compileAnswerToHtml :: Meta -> FilePath -> Answer -> Action Answer
    compileAnswerToHtml meta base mc@MultipleChoice {} = do
      traverseOf (answChoices . traverse . choiceTheAnswer) render mc
    compileAnswerToHtml meta base ma@MultipleAnswers {} = do
      traverseOf (answAnswers . traverse . oneDetail) render
        =<< traverseOf (answAnswers . traverse . oneCorrect) render ma
    compileAnswerToHtml meta base ff@FreeForm {} =
      traverseOf answCorrectAnswer render ff
    compileAnswerToHtml meta base nu@Numerical {} = return nu
    compileAnswerToHtml meta base ft@FillText {} = do
      traverseOf (answCorrectWords . traverse) render ft

-- | Renders a Markdown snippet to HTML applying the full Decker media filter.
renderSnippetToHtml :: Meta -> FilePath -> Text -> Action Text
renderSnippetToHtml meta base markdown = do
  pandoc <- liftIO $ handleError $ runPure $ readMarkdown pandocReaderOpts markdown
  let options = pandocWriterOpts {writerHTMLMathMethod = MathJax "Handled in the render"}
  filtered <-
    mergeDocumentMeta (setMetaValue "decker.use-data-src" False meta) pandoc
      >>= adjustResourcePathsA base
      -- >>= (\p -> print p >> return p)
      >>= deckerMediaFilter (Disposition Page Html) base
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

renderQuestionToHtml :: Int -> Text -> Question -> Html
renderQuestionToHtml h id quest = do
  H.div
    ! A.class_ "question"
    ! A.id (toValue id)
    $ do
      hn h $ do
        preEscapedText $ quest ^. qstTitle
        H.small $
          H.a
            ! A.class_ "vscode"
            ! A.href (toValue ("vscode://file" <> toString (quest ^. qstFilePath)))
            $ "(Edit)"
      H.div ! A.class_ "closed" $ do
        H.p $ preEscapedText $ quest ^. qstQuestion
        hn (h + 1) "Answer"
        H.p $ renderAnswerToHtml $ quest ^. qstAnswer
        H.table $ do
          H.tr $ do
            H.th "lecture id"
            H.td $ toHtml (quest ^. qstLectureId)
          H.tr $ do
            H.th "topic id"
            H.td $ toHtml (quest ^. qstTopicId)
          H.tr $ do
            H.th "path"
            H.td $
              H.code $
                H.a
                  ! A.class_ "vscode"
                  ! A.href (toValue ("vscode://file" <> toString (quest ^. qstFilePath)))
                  $ toHtml (quest ^. qstFilePath)

renderQuestionDocument :: Meta -> FilePath -> Question -> Action Text
renderQuestionDocument meta base quest = do
  htmlQuest <- compileQuestionToHtml meta base quest
  let html = renderQuestionToHtml 2 "" htmlQuest
  return $
    toText $
      renderHtml $
        H.html $ do
          H.head $ do
            H.meta ! A.charset "utf-8"
            H.script ! A.src "/support/vendor/mathjax/tex-svg.js" $ ""
            H.script ! A.src "/support/js/quest.js" $ ""
            H.link ! A.rel "stylesheet" ! A.href "/support/css/quest.css"
            H.title (preEscapedText $ quest ^. qstTitle)
          H.body html

renderQuestionBrowser :: FilePath -> [Question] -> Action Text
renderQuestionBrowser base questions = do
  return $
    toText $
      renderHtml $
        H.html $ do
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
        $ toHtml $ quest ^. qstTitle
    topicQuests =
      toHtml $
        concatMap
          ( \(lid, topics) ->
              map (questTitles lid) topics
          )
          grouped
    lectureTopics (lid, topics) =
      H.div
        ! A.dataAttribute "lecture" (toValue lid)
        $ toHtml $ map (topicButton . fst) topics
    questTitles lid (tid, quests) =
      H.div
        ! A.dataAttribute "lecture" (toValue lid)
        ! A.dataAttribute "topic" (toValue tid)
        $ toHtml $ map questButton quests

groupQuestions :: [Question] -> [(Text, [(Text, [Question])])]
groupQuestions questions = sorted
  where
    grouped :: HashMap Text (HashMap Text (NonEmpty Question))
    grouped = HashMap.map (groupBy _qstTopicId) (groupBy _qstLectureId questions)
    sorted :: [(Text, [(Text, [Question])])]
    sorted =
      List.sortOn fst $
        map
          ( \(k, v) ->
              ( k,
                List.sortOn fst $
                  map
                    ( \(k, v) ->
                        (k, List.sortOn _qstTitle $ NonEmpty.toList v)
                    )
                    $ HashMap.toList v
              )
          )
          $ HashMap.toList grouped

instance ToMarkup a => ToMarkup (NonEmpty a) where
  toMarkup = toHtml . map toMarkup . toList

renderQuestion :: Meta -> FilePath -> FilePath -> Action ()
renderQuestion meta src out =
  do
    putInfo $ "# render (for " <> out <> ")"
    liftIO (readQuestion src)
      >>= renderQuestionDocument meta (takeDirectory src)
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
