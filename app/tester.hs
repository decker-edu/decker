{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ()
import Control.Exception
import Data.Maybe ()
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as Map
import Data.Yaml.Pretty as Y
import Data.Hashable
import qualified Data.ByteString.Char8 as B
import Debug.Trace
import Development.Shake
import Development.Shake.FilePath
import System.FilePath ()
import System.FilePath.Glob
import System.Directory
import System.Random
import System.Random.Shuffle
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Highlighting.Kate.Styles
import Text.Pandoc
import Text.Pandoc.PDF
import Text.Pandoc.Walk
import Utilities
import Context
import Filter
import Test
import Student
import Embed

replaceSuffix srcSuffix targetSuffix filename = dropSuffix srcSuffix filename ++ targetSuffix

main :: IO ()
main = do
    -- Calculate some directories
    projectDir <- calcProjectDirectory
    let privateDir = projectDir </> "private"
    -- Find questions
    questionSources <- glob "**/*-quest.yaml"
    -- Find exams
    examSources <- glob "**/*-exam.yaml"
    let exams = 
            map
                (replaceSuffix "-exam.yaml" "-exam.pdf" .
                 combine privateDir . makeRelative projectDir)
                examSources
    -- Meta data
    metaFiles <- glob "**/*-meta.yaml"
    -- Calculate targets
    let catalog = privateDir </> "complete-quest-catalog.pdf"
    -- Prepare Mustache templates
    let templates = compileTesterTemplates
    --- 
    context <- makeActionContext projectDir privateDir "" ""
    runShakeInContext context shakeOptions $
        do do want ["catalog"]
              --
              catalog %>
                  \out -> do
                      allQuestions <- readQuestions questionSources
                      renderCatalog projectDir templates allQuestions out
              --
              phony
                  "catalog" $
                  need [catalog]
              --
              phony
                  "new-exam" $
                  do let string = Y.encodePretty Y.defConfig examStationary
                     liftIO $ B.writeFile "new-exam.yaml" string
              --
              phony
                  "new-mc" $
                  do let string = Y.encodePretty Y.defConfig multipleChoiceStationary
                     liftIO $ B.writeFile "new-mc-quest.yaml" string
              --
              phony
                  "new-ft" $
                  do let string = Y.encodePretty Y.defConfig fillTextStationary
                     liftIO $ B.writeFile "new-ft-quest.yaml" string
              --
              phony
                  "new-f" $
                  do let string = Y.encodePretty Y.defConfig freeStationary
                     liftIO $ B.writeFile "new-f-quest.yaml" string
              --
              phony
                  "exams" $
                  need exams
              --
              "//*-exam.pdf" %>
                  \out -> do
                      let examPath = 
                              (replaceSuffix "-exam.pdf" "-exam.yaml" .
                               combine projectDir . makeRelative privateDir)
                                  out
                      need $ [examPath]
                      questions <- readQuestions questionSources
                      exam <- readExamData examPath
                      let studentInfoPath = 
                              adjustLocalUrl
                                  projectDir
                                  (takeDirectory examPath)
                                  (examStudentInfoFile exam)
                      Students hashMap <- readStudentInfo studentInfoPath (examTrack exam)
                      renderExam exam questions (Map.elems hashMap) out
              --
              phony
                  "clean" $
                  removeFilesAfter "." ["private"]

-- | Filters questions by LectureIds and ExcludedTopicIds.
filterQuestions
    :: [T.Text] -> [T.Text] -> [(Question, FilePath)] -> [(Question, FilePath)]
filterQuestions includeLectures excludeTopics questions = 
    filter (not . (flip elem) excludeTopics . qstLectureId . fst) $
    filter ((flip elem) includeLectures . qstLectureId . fst) questions

-- | Groups questions first by LectureId and then by TopicId into nested HashMaps.
groupQuestions
    :: [(Question, FilePath)] -> Map.HashMap T.Text (Map.HashMap T.Text [(Question, FilePath)])
groupQuestions questions = 
    let byLectureId = foldl (groupBy qstLectureId) Map.empty questions
    in Map.map (foldl (groupBy qstTopicId) Map.empty) byLectureId
  where
    groupBy attrib rmap question = Map.insertWith (++) (attrib $ fst question) [question] rmap

renderExam :: Exam -> [(Question, FilePath)] -> [Student] -> FilePath -> Action ()
renderExam exam questions students out = do
    let candidates = filterQuestions (examLectureIds exam) (examExcludedTopicIds exam) questions
    mapM_ (renderExamFor candidates) students
    putNormal $ "Rendered exam: " ++ out
  where
    renderExamFor :: [(Question, FilePath)] -> Student -> Action ()
    renderExamFor candidates student = do
        -- | Initialize the RNG with a hash over the student data. 
        -- Should produce the identical exam for the student each time.
        let gen = mkStdGen (hash student)
        let questions = 
                take (examNumberOfQuestions exam) $ shuffle' candidates (length candidates) gen
        putNormal $ show (std_displayName student)
        mapM_ (putNormal . show . qstLectureId . fst) questions

-- | Throw, result is shitty.
maybeThrowYaml
    :: Y.FromJSON a
    => FilePath -> Either Y.ParseException a -> a
maybeThrowYaml _ (Right yaml) = yaml
maybeThrowYaml path (Left exception) = 
    throw $ YamlException $ "Error parsing YAML file: " ++ path ++ ", " ++ (show exception)

-- | Reads a YAML file or throws.
readYAML
    :: Y.FromJSON a
    => FilePath -> Action a
readYAML path = do
    result <- liftIO $ Y.decodeFileEither path
    let contents = 
            case result of
                Right yaml -> yaml
                Left exception -> 
                    throw $
                    YamlException $
                    "Error parsing YAML file: " ++
                    path ++ ", " ++ (Y.prettyPrintParseException exception)
    return contents

-- Reads exam data from file
readExamData
    :: FilePath -> Action Exam
readExamData = readYAML

-- Reads info from all participating students.
readStudentInfo
    :: FilePath -> Int -> Action Students
readStudentInfo path track = do
    need [path]
    Students hashMap <- readYAML path
    return $
        Students $
        Map.filter
            (\s -> 
                  std_track s == track)
            hashMap

-- Reads all the questions and returns them along with the base directory of
-- each.
readQuestions
    :: [FilePath] -> Action [(Question, FilePath)]
readQuestions files = mapM readQuestion files

readQuestion :: FilePath -> Action (Question, FilePath)
readQuestion file = do
    need [file]
    result <- liftIO $ Y.decodeFileEither file
    let question = 
            case result of
                Right yaml -> yaml
                Left exception -> 
                    throw $
                    YamlException $ "Error parsing YAML file: " ++ file ++ ", " ++ (show exception)
    return (question, takeDirectory file)

-- Renders a catalog of all questions (TODO sorted by LectureId and TopicId).
renderCatalog
    :: FilePath -> Templates -> [(Question, FilePath)] -> FilePath -> Action ()
renderCatalog projectDir templates questions out = do
    let markdown = 
            map
                (\(q,b) -> 
                      (renderMarkdown q, b))
                questions
    let pandoc = map parseMarkdown markdown
    need $ concatMap extractLocalImagePathes pandoc
    let catalog = 
            Pandoc nullMeta $
            concatMap
                (\(Pandoc _ blocks) -> 
                      blocks)
                pandoc
    let options = 
            def
            { writerStandalone = True
            , writerTemplate = B.unpack testLatexTemplate
            , writerHighlight = True
            , writerHighlightStyle = pygments
            , writerCiteMethod = Citeproc
            }
    putNormal $ "# pandoc (for " ++ out ++ ")"
    pandocMakePdf options catalog out
  where
    parseMarkdown (markdown,base) = 
        case readMarkdown def markdown of
            Left err -> throw $ PandocException (show err)
            Right pandoc -> walk (adjustImageUrls base) pandoc
    adjustImageUrls base (Image attr inlines (url,title)) = 
        (Image attr inlines (adjustLocalUrl projectDir base url, title))
    adjustImageUrls _ inline = inline
    renderMarkdown question = 
        T.unpack $ M.substitute (selectTemplate templates question) (MT.mFromJSON question)

-- TODO Make this work
newOrder
    :: Int -> IO [Int]
newOrder n = do
    gen <- getStdGen
    return $ shuffle' [0 .. (n - 1)] n gen

-- TODO Make this work
shuffleAnswers
    :: Question -> Action Question
shuffleAnswers q = 
    case qstAnswer q of
        MultipleChoice choices -> do
            let n = length choices
            order <- liftIO $ newOrder n
            return
                q
                { qstAnswer = MultipleChoice (shuffle choices order)
                }
        otherwise -> return q

examStationary :: Exam
examStationary = 
    Exam
    { examStudentInfoFile = "PATH/TO/STUDENT/INFO"
    , examDateTime = "DATE_TIME"
    , examDurationInMinutes = 0
    , examNumberOfQuestions = 0
    , examTrack = 1
    , examLectureIds = ["LECTURE_ID"]
    , examExcludedTopicIds = ["EXCLUDED_TOPIC_ID"]
    }

multipleChoiceStationary :: Question
multipleChoiceStationary = 
    Question
    { qstTopicId = "TOPIC_ID"
    , qstLectureId = "LECTURE_ID"
    , qstTitle = "MULTIPLE CHOICE"
    , qstPoints = 5
    , qstQuestion = "THE QUESTION?"
    , qstAnswer = MultipleChoice
      { answChoices = [ Choice "ANSWER_1" True
                      , Choice "ANSWER_2" True
                      , Choice "DISTRACTOR_1" False
                      , Choice "DISTRACTOR_2" False]
      }
    , qstDifficulty = Medium
    , qstComment = "COMMENT"
    }

fillTextStationary :: Question
fillTextStationary = 
    Question
    { qstTopicId = "TOPIC_ID"
    , qstLectureId = "LECTURE_ID"
    , qstTitle = "FILL TEXT"
    , qstPoints = 5
    , qstQuestion = "THE QUESTION?"
    , qstAnswer = FillText
      { answFillText = "FILL THE ___ IN THE ___."
      , answCorrectWords = ["HOLES", "TEXT"]
      }
    , qstDifficulty = Medium
    , qstComment = "COMMENT"
    }

freeStationary :: Question
freeStationary = 
    Question
    { qstTopicId = "TOPIC_ID"
    , qstLectureId = "LECTURE_ID"
    , qstTitle = "FREE"
    , qstPoints = 5
    , qstQuestion = "THE QUESTION?"
    , qstAnswer = FreeForm
      { answHeightInMm = 20
      , answCorrectAnswer = "THE ANSWER."
      }
    , qstDifficulty = Medium
    , qstComment = "COMMENT"
    }