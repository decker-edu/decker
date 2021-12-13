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
import Relude
import Relude.Extra.Group
import System.FilePath.Posix
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Exam.Question
import Text.Decker.Filter.Paths
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Reader.Markdown
-- import Text.Groom
import Text.Pandoc
import Text.Pandoc.Walk
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
    compileAnswerToHtml meta base ff@FreeForm {} = return ff
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
      >>= deckerMediaFilter (Disposition Page Html) base
  liftIO $ handleError $ runPure $ writeHtml5String options $ walk dropPara filtered

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
        H.td (preEscapedText $ one ^. oneDetail)
        H.td (preEscapedText $ one ^. oneCorrect)
renderAnswerToHtml answer@FreeForm {} = do
  H.text $ answer ^. answCorrectAnswer
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
        H.button "" -- ▼
        preEscapedText $ quest ^. qstTitle
        H.a
          ! A.class_ "vscode"
          ! A.href (toValue ("vscode://file" <> toString (quest ^. qstFilePath)))
          $ ""
      H.div ! A.class_ "closed" $ do
        H.p $ preEscapedText $ quest ^. qstQuestion
        H.p $ renderAnswerToHtml $ quest ^. qstAnswer

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
            H.style "img {width:100%;}"
            H.script ! A.src "/support/vendor/mathjax/tex-svg.js" $ ""
            H.script ! A.src "/support/examiner/reload.js" $ ""
            H.title (preEscapedText $ quest ^. qstTitle)
          H.body html

renderQuestionCatalog :: FilePath -> [Question] -> Action Text
renderQuestionCatalog base questions = do
  return $
    toText $
      renderHtml $
        H.html $ do
          H.head $ do
            H.meta ! A.charset "utf-8"
            H.title "Question Catalog"
            H.script ! A.type_ "module" ! A.src "/support/examiner/catalog.js" $ ""
            H.script ! A.src "/support/vendor/mathjax/tex-svg.js" $ ""
            H.script ! A.src "/support/examiner/reload.js" $ ""
            H.link ! A.rel "stylesheet" ! A.href "/support/examiner/catalog.css"
          H.body $ do
            H.header $
              H.h1 ("Question Catalog (" <> show (length questions) <> ")")
            rendered
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
    rendered :: Html
    rendered = toHtml $ map lecture sorted
    lecture (lid, topics) = do
      H.div
        ! A.class_ "lecture"
        ! A.id (toValue lid)
        $ do
          H.h1 $ do
            H.button "" -- ▼
            toHtml (lid <> " (" <> show (length topics) <> ")")
          H.div ! A.class_ "closed" $
            toHtml $ map (topic lid) topics
    topic lid (tid, quests) = do
      H.div
        ! A.class_ "topic"
        ! A.id (toValue (tid <> "-" <> lid))
        $ do
          H.h2 $ do
            H.button "▶" -- ▼
            toHtml (tid <> " (" <> show (length quests) <> ")")
          H.div ! A.class_ "closed" $
            toHtml $ map (quest 3 lid tid) $ zip [0 ..] quests
    quest hn lid tid (i, q) =
      let id = lid <> "-" <> tid <> "-" <> show i
       in renderQuestionToHtml 3 id q

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
      >>= renderQuestionCatalog base
      >>= (liftIO . Text.writeFile out)
