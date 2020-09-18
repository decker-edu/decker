{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Filter.Quiz
  ( handleQuizzes
  ) where

import Control.Exception
import Control.Lens hiding (Choice)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Walk

import qualified Data.Map.Strict as M
import Text.Decker.Filter.Local
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

-- Pair: consisting of a bucket where items should be dropped; The items which belong to the bucket
-- Distractor: Just a list of items without accompanying bucket
data Match
  = Pair
      { bucketID :: Int
      , bucket :: [Inline]
      , items :: [[Block]]
      }
  | Distractor
      { items :: [[Block]]
      }
  deriving (Show)

-- | A Choice consists of a Boolean (correct), the answer text and a tooltip comment
data Choice =
  Choice
    { correct :: Bool
    , text :: [Inline]
    , comment :: [Block]
    }
  deriving (Show)

-- | Set different (optional) meta options for quizzes in a yaml code block
data QuizMeta =
  QuizMeta
    { _category :: T.Text
    , _lectureId :: T.Text
    , _score :: Int
    , _topic :: T.Text
    , _lang :: T.Text
    }
  deriving (Show)

makeLenses ''QuizMeta

-- | The Quiz datatype.
-- Each quiz type contains: title, tags/classes, meta data and the questions+answers
data Quiz
  = MultipleChoice
  -- Multiple Choice questions consist of one question (e.g. h2 header and some blocks) and a following choices/selection part
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _quizMeta :: QuizMeta
      , _question :: [Block]
      , _choices :: [Choice]
      }
  | MatchItems
  -- Matching Questions consist of one question and a pairing "area" for sorting items via dragging and dropping
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _quizMeta :: QuizMeta
      , _question :: [Block]
      , _pairs :: [Match]
      }
  | InsertChoices
  -- These questions can have multiple question and answer/choices parts. 
  -- This is why questions is a list of tuples. 
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _quizMeta :: QuizMeta
      , _questions :: [([Block], [Choice])]
      }
  | FreeText
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _quizMeta :: QuizMeta
      , _question :: [Block]
      , _choices :: [Choice]
      }
  deriving (Show)

makeLenses ''Quiz

-- | Has to be called in the Markdown.hs deckerPipeline after processSlides
-- | Depends on h2 headers being wrapped in boxes
handleQuizzes :: Pandoc -> Decker Pandoc
handleQuizzes pandoc@(Pandoc meta blocks) = return $ walk parseQuizboxes pandoc
  where
    parseQuizboxes :: Block -> Block
    parseQuizboxes d@(Div (id_, tgs, kvs) blocks)
      | any (`elem` tgs) ["qmi", "quiz-mi", "quiz-match-items"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultMatch tgs) blocks)
      | any (`elem` tgs) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultMC tgs) blocks)
      | any (`elem` tgs) ["qic", "quiz-ic", "quiz-insert-choices"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultIC tgs) blocks)
      | any (`elem` tgs) ["qft", "quiz-ft", "quiz-free-text"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultFree tgs) blocks)
      | otherwise = d
    parseQuizboxes bl = bl
    -- Give the tag-/classlist of the surrounding div box to the quiz
    setTags :: Quiz -> [T.Text] -> Quiz
    setTags q ts =
      if elem "columns" ts
        then set tags ts q
        else set tags (ts ++ ["columns", "box"]) q
    -- The default "new" quizzes
    defaultMeta = QuizMeta "" "" 0 "" (lookupMetaOrElse "en" "lang" meta)
    defaultMatch = MatchItems [] [] defaultMeta [] []
    defaultMC = MultipleChoice [] [] defaultMeta [] []
    defaultIC = InsertChoices [] [] defaultMeta []
    defaultFree = FreeText [] [] defaultMeta [] []

-- Take the parsed Quizzes and render them to html
renderQuizzes :: Meta -> Quiz -> Block
renderQuizzes meta quiz@MatchItems {} = renderMatching meta quiz
renderQuizzes meta quiz@FreeText {} = renderFreeText meta quiz
renderQuizzes meta quiz@InsertChoices {} = renderInsertChoices meta quiz
renderQuizzes meta quiz@MultipleChoice {} = renderMultipleChoice meta quiz

-- Takes a Quiz and a list of blocks, parses the blocks and modifies the given quiz
parseAndSetQuiz :: Quiz -> [Block] -> Quiz
parseAndSetQuiz q bs = combineICQuestions (foldl parseAndSetQuizFields q bs)

-- | This function combines the (Question, Choices) tuples of InsertChoice questions
-- These tuples are of type ([Block], [Choice])
-- This is because in `parseAndSetQuizFields` they are created alternatingly as (bs, []) and ([],chs)
-- This combine function makes it so that the questions field in InsertChoice is 
-- an alternating list of "Question text -> quiz element -> question text" etc
combineICQuestions :: Quiz -> Quiz
combineICQuestions quiz@(InsertChoices ti tgs qm q) =
  set questions (combineQTuples q) quiz
    -- These tuples can only be ([],a) or (a,[]) per default
    -- They're created like this in parseAndSetQuizFields
  where
    combineQTuples :: [([Block], [Choice])] -> [([Block], [Choice])]
    combineQTuples [] = []
    combineQTuples bc@[(a, b)] = bc
    -- combine two question blocks
    combineQTuples ((a, []):(b, []):rest) = combineQTuples ((a ++ b, []) : rest)
    -- combine Question with a choice block
    combineQTuples ((a, []):([], b):rest) = (a, b) : combineQTuples rest
    -- If the head has anything other than an empty choice then ignore it
    combineQTuples (a:(y, b):rest) = a : combineQTuples ((y, b) : rest)
combineICQuestions q = q

-- | This monolithic function parses a Pandoc Block and uses lenses to set the field in the given quiz item
parseAndSetQuizFields :: Quiz -> Block -> Quiz
-- Set the title
parseAndSetQuizFields q (Header 2 (id_, cls, kvs) text) = set title text q
-- Set the meta information
parseAndSetQuizFields q (CodeBlock (id_, cls, kvs) code) =
  if "yaml" `elem` cls
    then setQuizMeta q (decodeYaml code)
    else q
  where
    decodeYaml :: T.Text -> Meta
    decodeYaml text =
      case decodeEither' (encodeUtf8 text) of
        Right a -> toPandocMeta a
        Left exception -> Meta M.empty
-- Set quiz pairs/Match Items
-- Zip with index
parseAndSetQuizFields quiz@MatchItems {} (DefinitionList items) =
  set pairs (map parseDL (zip [1 ..] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, [Plain (Str "!":inl)]:bs)) = Pair i is [[Plain []]]
    parseDL (i, (is, bs)) = Pair i is bs
-- parse and set choices for FreeText
parseAndSetQuizFields quiz@FreeText {} (BulletList blocks) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- parse and Set choices for InsertChoices
parseAndSetQuizFields quiz@(InsertChoices ti tgs qm q) (BulletList blocks) =
  set questions (q ++ [([], map (parseQuizTLItem quiz) blocks)]) quiz
-- Parse and set choices for MultipleChoice
parseAndSetQuizFields quiz@MultipleChoice {} (BulletList blocks) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- The questions are prepended
-- Parse and set question for matching
parseAndSetQuizFields quiz@(MatchItems ti tgs qm q p) b =
  set question (q ++ [b]) quiz
-- Parse and set questions for FreeText
parseAndSetQuizFields quiz@(FreeText ti tgs qm q ch) b =
  set question (q ++ [b]) quiz
-- Set question for InsertChoices
parseAndSetQuizFields quiz@(InsertChoices ti tgs qm q) b =
  set questions (q ++ [([b], [])]) quiz
-- Set question for Multiple Choice
parseAndSetQuizFields quiz@(MultipleChoice ti tgs qm q ch) b =
  set question (q ++ [b]) quiz

-- | Parse a Pandoc Bullet/Task list item to a Choice
-- Pandoc replaces [X] internally with unicode checkboxes before quiz parsing
-- This is why we pattern match for "☒" and "☐" here
parseQuizTLItem :: Quiz -> [Block] -> Choice
parseQuizTLItem _ (Plain (Str "☒":Space:is):bs) = Choice True is bs
parseQuizTLItem _ (Plain (Str "☐":Space:is):bs) = Choice False is bs
parseQuizTLItem FreeText {} (Plain is:bs) = Choice True is bs
-- parseQuizTLItem InsertChoices {} (Plain is:bs) = Choice True is bs
parseQuizTLItem _ is = Choice False [Str "Error: NoTasklistItem"] [Plain []]

-- | Set the quizMeta field of a Quiz using lenses
-- available meta options are hardcoded here
setQuizMeta :: Quiz -> Meta -> Quiz
setQuizMeta q meta = set quizMeta (setMetaForEach meta (q ^. quizMeta)) q
  where
    setMetaForEach :: Meta -> QuizMeta -> QuizMeta
    setMetaForEach m qm =
      foldr (setMeta' m) qm ["score", "category", "lectureId", "topic", "lang"]
    setMeta' :: Meta -> T.Text -> QuizMeta -> QuizMeta
    setMeta' m t qm =
      case t of
        "score" -> set score (lookupMetaOrElse 0 t m) qm
        "category" -> set category (lookupMetaOrElse "" t m) qm
        "lectureId" -> set lectureId (lookupMetaOrElse "" t m) qm
        "topic" -> set topic (lookupMetaOrElse "" t m) qm
        "lang" -> set lang (lookupMetaOrElse (view lang qm) t m) qm
        _ -> throw $ InternalException $ "Unknown meta data key: " <> show t

-- | A simple Html button
solutionButton :: Meta -> Block
solutionButton meta =
  rawHtml' $ do
    H.button ! A.class_ "solutionButton" $ H.toHtml buttonText
  where
    buttonText :: T.Text
    buttonText = lookupInDictionary "quiz.solution-button" meta

renderMultipleChoice :: Meta -> Quiz -> Block
renderMultipleChoice meta quiz@(MultipleChoice title tgs qm q ch) =
  Div ("", tgs, []) $ header ++ q ++ [choiceBlock]
  -- ++ [solutionButton]
  where
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    choiceBlock = rawHtml' $ choiceList "choices" ch
renderMultipleChoice meta q =
  Div ("", [], []) [Para [Str "ERROR NO MULTIPLE CHOICE QUIZ"]]

-- | Transform a list of Choices to an HTML <ul>
-- This is used directly in multiple choice questions and as "solutionList" in IC and FT questions
choiceList :: AttributeValue -> [Choice] -> Html
choiceList t choices =
  H.ul ! A.class_ t $
  foldr ((>>) . handleChoices) (H.span $ H.toHtml ("" :: T.Text)) choices
  where
    reduceTooltip :: [Block] -> [Block]
    reduceTooltip [BulletList blocks] -- = concat blocks
     = concatMap (\x -> x ++ [Plain [LineBreak]]) blocks
    reduceTooltip bs = bs
    handleChoices :: Choice -> Html
    handleChoices (Choice correct text comment) =
      if correct
        then H.li ! A.class_ "correct" $ do
               toHtml text
               H.div ! A.class_ "tooltip" $ toHtml (reduceTooltip comment)
        else H.li ! A.class_ "wrong" $ do
               toHtml text
               H.div ! A.class_ "tooltip" $ toHtml (reduceTooltip comment)

renderInsertChoices :: Meta -> Quiz -> Block
renderInsertChoices meta quiz@(InsertChoices title tgs qm q) =
  Div ("", tgs, []) $ header ++ questionBlocks q ++ tooltipDiv
  -- ++ [solutionButton]
  where
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    tooltipDiv = [Div ("", [T.pack "tooltip-div"], []) []]
    questionBlocks :: [([Block], [Choice])] -> [Block]
    questionBlocks = map (rawHtml' . handleTuple)
    handleTuple :: ([Block], [Choice]) -> Html
    handleTuple ([], chs) = select chs
    handleTuple (bs, []) = toHtml (map reduceBlock bs)
    handleTuple (bs, chs) = toHtml (map reduceBlock bs) >> select chs
    reduceBlock :: Block -> Block
    reduceBlock (Para is) = Plain ([Str " "] ++ is ++ [Str " "])
    reduceBlock p = p
    select :: [Choice] -> Html
    select choices =
      (H.select $
       (H.option ! A.class_ "wrong" $ H.toHtml ("..." :: T.Text)) >>
       (foldr ((>>) . options) H.br choices)) >>
      choiceList "solutionList" choices
    options :: Choice -> Html
    options (Choice correct text comment) =
      if correct
        then H.option ! A.class_ "correct" $ toHtml $ stringify text
        else H.option ! A.class_ "wrong" $ toHtml $ stringify text
renderInsertChoices meta q =
  Div ("", [], []) [Para [Str "ERROR NO INSERT CHOICES QUIZ"]]

-- 
renderMatching :: Meta -> Quiz -> Block
renderMatching meta quiz@(MatchItems title tgs qm qs matches) =
  Div ("", tgs, []) $ header ++ qs ++ [itemsDiv, bucketsDiv, button]
  where
    newMeta = setMetaValue "lang" (view lang qm) meta
    button = solutionButton newMeta
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    (buckets, items) = unzip $ map pairs matches
    dropHint = ("data-hint", lookupInDictionary "quiz.qmi-drop-hint" newMeta)
    dragHint = ("data-hint", lookupInDictionary "quiz.qmi-drag-hint" newMeta)
    itemsDiv = Div ("", ["matchItems"], [dragHint]) (concat items)
    bucketsDiv = Div ("", ["buckets"], [dropHint]) buckets
    item :: T.Text -> [Block] -> Block
    item index =
      Div ("", ["matchItem"], [("draggable", "true"), ("bucketId", index)])
    distractor :: [Block] -> Block
    distractor = Div ("", ["matchItem", "distractor"], [("draggable", "true")])
    pairs :: Match -> (Block, [Block])
    pairs (Distractor bs) = (Text.Pandoc.Definition.Null, map distractor bs)
    pairs (Pair i is bs) =
      case bs of
        [[Plain []]] ->
          ( Div
              ("", ["bucket", "distractor"], [("bucketId", T.pack $ show i)])
              [Plain is]
          , [])
        _ ->
          ( Div ("", ["bucket"], [("bucketId", T.pack $ show i)]) [Plain is]
          , map (item (T.pack $ show i)) bs)
renderMatching meta q = Div ("", [], []) [Para [Str "ERROR NO MATCHING QUIZ"]]

renderFreeText :: Meta -> Quiz -> Block
renderFreeText meta quiz@(FreeText title tgs qm q ch) =
  Div ("", tgs, []) $ header ++ q ++ [inputRaw] ++ [button]
  where
    newMeta = setMetaValue "lang" (view lang qm) meta
    button = solutionButton newMeta
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    placeholderText :: T.Text
    placeholderText = lookupInDictionary "quiz.input-placeholder" newMeta
    inputRaw =
      rawHtml'
        ((H.input ! A.placeholder (H.textValue placeholderText)) >>
         choiceList "solutionList" ch)
renderFreeText meta q = Div ("", [], []) [Para [Str "ERROR NO FREETEXT QUIZ"]]
