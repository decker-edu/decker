{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Filter.Quiz
  ( handleQuizzes,
    Quiz (..),
    Match (..),
    Choice (..),
    QuizMeta (..),
  )
where

import Control.Exception ( throw )
import Control.Lens ( view, (^.), set, makeLenses )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding as E ( encodeUtf8 )
import Data.Yaml ( decodeEither' )
import Text.Decker.Internal.Common ( Decker )
import Text.Decker.Internal.Meta
import Text.Decker.Filter.Slide (tag)
import Text.Pandoc.Definition
import Text.Pandoc.Shared ( stringify )
import Text.Pandoc.Walk ( Walkable(walk) )
import Text.DocTemplates (Doc(Block))

-- Pair: consisting of a bucket where items should be dropped; The items which belong to the bucket
-- Distractor: Just a list of items without accompanying bucket
data Match
  = Pair {bucketID :: Int, bucket :: [Inline], items :: [[Block]]}
  | Distractor {items :: [[Block]]}
  deriving (Show)

-- | A Choice consists of a Boolean (correct), the answer text and a tooltip comment
data Choice = Choice 
  { correct :: Bool
  , text :: [Inline]
  , comment :: [Block]
  } deriving (Show)

-- | Set different (optional) meta options for quizzes in a yaml code block
data QuizMeta = QuizMeta
  { _category :: T.Text,
    _lectureId :: T.Text,
    _score :: Int,
    _topic :: T.Text,
    _lang :: T.Text,
    _style :: T.Text,
    _solution :: T.Text
  }
  deriving (Show)

makeLenses ''QuizMeta

-- | The Quiz datatype.
-- Each quiz type contains: title, tags/classes, meta data and the questions+answers
data Quiz
  = MultipleChoice
      { -- Multiple Choice questions consist of one question (e.g. h2 header and some blocks) and a following choices/selection part
        _title :: [Inline],
        _tags :: [T.Text],
        _quizMeta :: QuizMeta,
        _question :: [Block],
        _choices :: [Choice]
      }
  | MatchItems
      { -- Matching Questions consist of one question and a pairing "area" for sorting items via dragging and dropping
        _title :: [Inline],
        _tags :: [T.Text],
        _quizMeta :: QuizMeta,
        _question :: [Block],
        _pairs :: [Match]
      }
  | InsertChoices
      { -- These questions can have multiple question and answer/choices parts.
        -- This is why questions is a list of tuples.
        _title :: [Inline],
        _tags :: [T.Text],
        _quizMeta :: QuizMeta,
        _questions :: [([Block], [Choice])]
      }
  | FreeText
      { _title :: [Inline],
        _tags :: [T.Text],
        _quizMeta :: QuizMeta,
        _question :: [Block],
        _choices :: [Choice]
      }
  deriving (Show)

makeLenses ''Quiz

-- | Has to be called in the Markdown.hs deckerPipeline after processSlides
-- | Depends on h2 headers being wrapped in boxes
handleQuizzes :: Pandoc -> Decker Pandoc
handleQuizzes pandoc@(Pandoc meta blocks) = return $ walk parseQuizboxes pandoc
  where
    parseQuizboxes :: Block -> Block
    parseQuizboxes d@(Div (id_, cls, kvs) blocks)
      | any (`elem` cls) ["qmi", "quiz-mi", "quiz-match-items"] =
        renderQuizzes
          meta
          (parseAndSetQuiz (setTags defaultMatch cls) blocks)
      | any (`elem` cls) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultMC cls) blocks)
      | any (`elem` cls) ["qic", "quiz-ic", "quiz-insert-choices"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultIC cls) blocks)
      | any (`elem` cls) ["qft", "quiz-ft", "quiz-free-text"] =
        renderQuizzes meta (parseAndSetQuiz (setTags defaultFree cls) blocks)
      | otherwise = d
    parseQuizboxes bl = bl
    -- Give the classlist of the surrounding div box to the quiz
    -- Style is set later
    setTags :: Quiz -> [T.Text] -> Quiz
    setTags q cls = do
      let cls' = filter (\st -> st /= "fancy" && st /= "plain") cls
      if "columns" `elem` cls'
        then set tags cls' q
        else set tags (cls' ++ ["columns", "box"]) q
    -- The default "new" quizzes
    defaultMeta = QuizMeta "" "" 0 "" (lookupMetaOrElse "en" "lang" meta) (lookupMetaOrElse "fancy" "quiz.style" meta) (lookupMetaOrElse "" "quiz.solution" meta)
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
combineICQuestions quiz@(InsertChoices ti tgs qm q) = set questions (combineQTuples q) quiz
  where
    -- These tuples can only be ([],a) or (a,[]) per default
    -- They're created like this in parseAndSetQuizFields
    combineQTuples ::
      [([Block], [Choice])] -> [([Block], [Choice])]
    combineQTuples [] = []
    combineQTuples bc@[(a, b)] = bc
    -- combine two question blocks
    combineQTuples ((a, []) : (b, []) : rest) =
      combineQTuples ((a ++ b, []) : rest)
    -- combine Question with a choice block
    combineQTuples ((a, []) : ([], b) : rest) =
      (a, b) : combineQTuples rest
    -- If the head has anything other than an empty choice then ignore it
    combineQTuples (a : (y, b) : rest) = a : combineQTuples ((y, b) : rest)
combineICQuestions q = q

-- | This monolithic function parses a Pandoc Block and uses lenses to set the field in the given quiz item
parseAndSetQuizFields :: Quiz -> Block -> Quiz
-- Set the title and quiz style if specified per question
parseAndSetQuizFields q (Header 2 (id_, cls, kvs) text)
  | "fancy" `elem` cls = set quizMeta (set style "fancy" (q ^. quizMeta)) $ set title text q
  | "plain" `elem` cls = set quizMeta (set style "plain" (q ^. quizMeta)) $ set title text q
-- Set the meta information
parseAndSetQuizFields q (CodeBlock (id_, cls, kvs) code) =
  if "yaml" `elem` cls then setQuizMeta q (decodeYaml code) else q
  where
    decodeYaml :: T.Text -> Meta
    decodeYaml text =
      case decodeEither' (encodeUtf8 text) of
        Right a -> toPandocMeta a
        Left exception -> Meta M.empty
-- Set quiz pairs/Match Items
-- Zip with index
parseAndSetQuizFields quiz@MatchItems {} (DefinitionList items) =
  set pairs (zipWith (curry parseDL) [1 .. ] items) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!" : _, bs)) = Distractor bs
    parseDL (i, (is, [Plain (Str "!" : inl)] : bs)) =
      Pair i is [[Plain []]]
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
parseQuizTLItem :: Quiz -> [Block] -> Choice
parseQuizTLItem _ (Plain (Str "\9746" : Space : is) : bs) = Choice True is bs
parseQuizTLItem _ (Plain (Str "\9744" : Space : is) : bs) = Choice False is bs
parseQuizTLItem FreeText {} (Plain is : bs) = Choice True is bs
-- parseQuizTLItem MultipleChoice {} a = Choice True [Str "Testing MC"] a
parseQuizTLItem _ is = Choice False [Str "Error: NoTasklistItem"] [Plain []]

-- | Set the quizMeta field of a Quiz using lenses
-- available meta options are hardcoded here
setQuizMeta :: Quiz -> Meta -> Quiz
setQuizMeta q meta = set quizMeta (setMetaForEach meta (q ^. quizMeta)) q
  where
    setMetaForEach :: Meta -> QuizMeta -> QuizMeta
    setMetaForEach m qm =
      foldr
        (setMeta' m)
        qm
        ["score", "category", "lectureId", "topic", "lang", "quiz.style", "quiz.solution"]
    setMeta' :: Meta -> T.Text -> QuizMeta -> QuizMeta
    setMeta' m t qm =
      case t of
        "score" -> set score (lookupMetaOrElse 0 t m) qm
        "category" -> set category (lookupMetaOrElse "" t m) qm
        "lectureId" -> set lectureId (lookupMetaOrElse "" t m) qm
        "topic" -> set topic (lookupMetaOrElse "" t m) qm
        "lang" -> set lang (lookupMetaOrElse (view lang qm) t m) qm
        "quiz.style" -> set style (lookupMetaOrElse (view style qm) t m) qm
        "quiz.solution" -> set solution (lookupMetaOrElse (view solution qm) t m) qm
        _ -> throw $ InternalException $ "Unknown meta data key: " <> show t

-- | A simple Html button
quizButton :: T.Text -> T.Text -> Meta -> Inline 
quizButton cls dict meta = 
  tag "button" $ Span ("", [cls, "quiz-button"], []) [Str $ lookupInDictionary dict meta]

renderMultipleChoice :: Meta -> Quiz -> Block
renderMultipleChoice meta quiz@(MultipleChoice title tgs qm q ch) =
  Div ("", cls, []) $ header ++ q ++ [choiceBlock]
  -- Div ("", cls, []) $ header ++ q ++ [Plain [choiceBlock]]
  where
    cls = tgs ++ [view style qm] ++ [view solution qm]
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    choiceBlock = choiceList "choices" ch
renderMultipleChoice meta q =
  Div ("", [], []) [Para [Str "ERROR NO MULTIPLE CHOICE QUIZ"]]

-- | Build <ul> list of choices
choiceList :: T.Text -> [Choice] -> Block
choiceList t choices = tag "ul" $ Div ("", [t], []) $ map handleChoices choices
  where
    handleChoices :: Choice -> Block
    handleChoices (Choice c text comment) = 
      tag "li" $ 
      Div ("", [cls c], []) $ 
      Div ("", ["choice_ltr"], []) [Plain text] : [Div ("", ["quiz-tooltip"], []) (reduceTooltip comment)]
    cls cor = if cor then "correct" else "wrong"
    reduceTooltip :: [Block] -> [Block]
    reduceTooltip [BulletList blocks] = concatMap (\x -> x ++ [Plain [LineBreak]]) blocks
    reduceTooltip bs = bs


renderInsertChoices :: Meta -> Quiz -> Block
renderInsertChoices meta quiz@(InsertChoices title tgs qm q) =
  Div ("", cls, []) $ header ++ buildQuestions q ++ tooltipDiv
  where
    cls = tgs ++ [view style qm] ++ [view solution qm]
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    buildQuestions :: [([Block], [Choice])] -> [Block]
    buildQuestions = concatMap questionBlocks
    questionBlocks :: ([Block], [Choice]) -> [Block]
    questionBlocks ([], chs) = Plain [select chs] : [choiceList "quiz-hidden" chs]
    questionBlocks (bs, []) = map reduceBlock bs
    questionBlocks (bs, chs) = map reduceBlock bs ++ Plain [select chs] : [choiceList "quiz-hidden" chs]
    reduceBlock :: Block -> Block
    reduceBlock (Para is) = Plain ([Str " "] ++ is ++ [Str " "])
    reduceBlock p = p
    select :: [Choice] -> Inline
    select choices =
      tag "select" $ Span ("", [], []) (defaultOpt : map options choices)
    defaultOpt = tag "option" $ Span ("", ["wrong"], []) [Str "..."]
    options :: Choice -> Inline
    options (Choice correct text comment) =
      tag "option" $ Span ("", [ocls], [("value",stringify text)]) text
      where
        ocls = if correct then "correct" else "wrong"
    tooltipDiv = [Div ("", [T.pack "tooltip-div"], []) []]
renderInsertChoices meta q =
  Div ("", [], []) [Para [Str "ERROR NO INSERT CHOICES QUIZ"]]

--
renderMatching :: Meta -> Quiz -> Block
renderMatching meta quiz@(MatchItems title tgs qm qs matches) =
  Div ("", cls, []) $ header ++ qs ++ [itemsDiv, bucketsDiv, Plain [sButton]]
  where
    cls = tgs ++ [view style qm] ++ [view solution qm]
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    itemsDiv = Div ("", ["matchItems"], [dragHint]) (concat items)
    dragHint = ("data-hint", lookupInDictionary "quiz.qmi-drag-hint" newMeta)
    newMeta = setMetaValue "lang" (view lang qm) meta
    (buckets, items) = unzip $ map pairs matches
    pairs :: Match -> (Block, [Block])
    pairs (Distractor bs) = (Null, map distractor bs)
    pairs (Pair i is bs) =
      case bs of
        [[Plain []]] -> (Div ("",["bucket", "distractor"],[("data-bucketId", T.pack $ show i)]) [Plain is], [])
        _ -> (Div ("",["bucket"],[("data-bucketId", T.pack $ show i)]) [Plain is], map (item (T.pack $ show i)) bs)
    distractor :: [Block] -> Block
    distractor = Div ("",["matchItem", "distractor"],[("draggable", "true")])
    item :: T.Text -> [Block] -> Block
    item index = Div ("",["matchItem"],[("draggable", "true"), ("data-bucketId", index)])
    bucketsDiv = Div ("", ["buckets"], [dropHint]) buckets
    dropHint = ("data-hint", lookupInDictionary "quiz.qmi-drop-hint" newMeta)
    sButton = quizButton "solutionButton" "quiz.solution" newMeta
renderMatching meta q =
  Div ("", [], []) [Para [Str "ERROR NO MATCHING QUIZ"]]

renderFreeText :: Meta -> Quiz -> Block
renderFreeText meta quiz@(FreeText title tgs qm q ch) =
  Div ("", cls, []) $ header ++ q ++ [input] ++ [Plain (sButton : [rButton] )]
  where
    cls = tgs ++ [view style qm] ++ [view solution qm]
    header =
      case title of
        [] -> []
        _ -> [Header 2 ("", [], []) title]
    input = tag "input" $ Div ("", ["quiz-ftinput"], [("placeholder", placeholderText)]) [choiceList "qft-solutions" ch]
    placeholderText :: T.Text
    placeholderText = lookupInDictionary "quiz.input-placeholder" newMeta
    sButton = quizButton "solutionButton" "quiz.solution" newMeta 
    rButton = quizButton "resetButton" "quiz.reset-button" newMeta
    newMeta = setMetaValue "lang" (view lang qm) meta
    -- sol = tag "ul" $ Span ("", ["solutionDiv"], []) [Str ""]
renderFreeText meta q =
  Div ("", [], []) [Para [Str "ERROR NO FREETEXT QUIZ"]]
