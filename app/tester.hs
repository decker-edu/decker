{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Exception
import Data.Maybe ()
import Data.List
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as Map
import Data.Yaml.Pretty as Y
import Data.Hashable
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Debug.Trace
import Development.Shake
import Development.Shake.FilePath
import System.Process
import System.Exit
import System.FilePath ()
import System.FilePath.Glob
import System.Directory
import System.Random
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
import Shuffle

replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

main :: IO ()
main = do
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
  let solutions =
        map
          (replaceSuffix "-exam.yaml" "-solution.pdf" .
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
    do want ["catalog"]
       --
       catalog %>
         \out -> do
           allQuestions <- readQuestions questionSources
           renderCatalog
             projectDir
             templates
             (sortOn (qstLectureId . fst) allQuestions)
             out
       --
       phony "catalog" $ need [catalog]
       --
       phony "new-exam" $
         do let string = Y.encodePretty Y.defConfig examStationary
            liftIO $ B.writeFile "new-exam.yaml" string
       --
       phony "new-mc" $
         do let string = Y.encodePretty Y.defConfig multipleChoiceStationary
            liftIO $ B.writeFile "new-mc-quest.yaml" string
       --
       phony "new-ma" $
         do let string = Y.encodePretty Y.defConfig multipleAnswersStationary
            liftIO $ B.writeFile "new-ma-quest.yaml" string
       --
       phony "new-ft" $
         do let string = Y.encodePretty Y.defConfig fillTextStationary
            liftIO $ B.writeFile "new-ft-quest.yaml" string
       --
       phony "new-f" $
         do let string = Y.encodePretty Y.defConfig freeStationary
            liftIO $ B.writeFile "new-f-quest.yaml" string
       --
       phony "exams" $ need exams
       --
       phony "solutions" $ need solutions
       --
       "//*-exam.pdf" %>
         \out -> do
           let examPath =
                 (replaceSuffix "-exam.pdf" "-exam.yaml" .
                  combine projectDir . makeRelative privateDir)
                   out
           need [examPath]
           buildExam projectDir "exam" examPath questionSources out
       --
       "//*-solution.pdf" %>
         \out -> do
           let examPath =
                 (replaceSuffix "-solution.pdf" "-exam.yaml" .
                  combine projectDir . makeRelative privateDir)
                   out
           buildExam projectDir "solution" examPath questionSources out
       --
       phony "clean" $ removeFilesAfter "." ["private"]

-- Calculate some directories
-- | Require a clean working tree to proceed
isWorkingTreeClean :: FilePath -> IO Bool
isWorkingTreeClean root = do
  let refreshIndex =
        "git -C " ++ root ++ " update-index -q --ignore-submodules --refresh"
  let unstangedChanges =
        "git -C " ++ root ++ " diff-files --quiet --ignore-submodules --"
  let uncommitedChanges =
        "git -C " ++
        root ++ " diff-index --cached --quiet HEAD --ignore-submodules --"
  system refreshIndex
  usc <- system unstangedChanges
  ucc <- system uncommitedChanges
  return (usc == ExitSuccess && ucc == ExitSuccess)

-- | Get the abbreviated id of the last commit.
lastCommitId :: IO String
lastCommitId = do
  r <- readProcess "git" ["--no-pager", "log", "-1", "--format=%H"] ""
  return $ take 8 r

-- | Return last commit id of clean working dir, or exit with failure.
cleanCommitIdOrFail :: FilePath -> Action String
cleanCommitIdOrFail root = do
  clean <- liftIO $ isWorkingTreeClean root
  if clean
    then throw $ GitException "Workspace dirty, aborting."
    else liftIO lastCommitId

--            buildExam projectDir "exam" examPath questionSources out
buildExam :: FilePath
          -> String
          -> FilePath
          -> [FilePath]
          -> FilePath
          -> Action ()
buildExam projectDir disposition examSource questionSources out = do
  need [examSource]
  putLoud "Reading questions ..."
  questions <- readQuestions questionSources
  putLoud "Reading exam data ..."
  exam <- readExamData examSource
  let studentInfoPath =
        adjustLocalUrl
          projectDir
          (takeDirectory examSource)
          (examStudentInfoFile exam)
  putLoud "Reading students ..."
  Students studentMap <- readStudentInfo studentInfoPath (examTrack exam)
  putLoud "Generating exam ..."
  let examData = generateExam exam questions (Map.elems studentMap)
  putLoud "About to show exam data ..."
  putLoud "Compiling templates ..."
  templates <- compileTemplates disposition
  putLoud "Compiling exam ..."
  examPandoc <- compileExam projectDir templates examData
  putLoud "Compiling PDF ..."
  compilePandocPdf examPandoc out

compileTemplates :: FilePath -> Action MT.TemplateCache
compileTemplates disposition = do
  let templateNames =
        [ "title-page.md"
        , "student-title-page.md"
        , "multiple-choice.md"
        , "multiple-answers.md"
        , "fill-text.md"
        , "free-form.md"
        ]
  compiled <- mapM (compileProjectTemplate disposition) templateNames
  return $ Map.fromList $ zip templateNames compiled

compileProjectTemplate :: FilePath -> FilePath -> Action MT.Template
compileProjectTemplate disposition name = do
  projectDir <- getProjectDir
  let filename = projectDir </> "exams" </> "templates" </> disposition </> name
  need [filename]
  text <- liftIO $ T.readFile filename
  let result = M.compileTemplate name (fixMustacheMarkupText text)
  return $
    case result of
      Right templ -> templ
      Left parseError ->
        throw $
        MustacheException $
        "Error parsing mustache template: " ++ show parseError

compilePandocPdf :: Pandoc -> FilePath -> Action ()
compilePandocPdf exam out = do
  let variables =
        [ ("documentclass", "scrartcl")
        , ("lang", "german")
        , ("babel-lang", "german")
        , ("classoption", "fontsize=13pt")
        ]
  let options =
        def
        { writerStandalone = True
        , writerVariables = variables
        , writerTemplate = examLatexTemplate
        , writerHighlight = True
        , writerHighlightStyle = pygments
        , writerCiteMethod = Citeproc
        }
  putNormal $ "# pandoc (for " ++ out ++ ")"
  pandocMakePdf options exam out

compileQuestion :: FilePath
                -> MT.TemplateCache
                -> (Question, FilePath)
                -> Action Pandoc
compileQuestion projectDir templates question = do
  putLoud "Compiling question ..."
  return $ compile question
  where
    compile :: (Question, FilePath) -> Pandoc
    compile (quest, base) =
      let rendered = renderMarkdown quest
      in case readMarkdown def rendered of
           Left err -> throw $ PandocException (show err)
           Right pandoc -> walk (adjustImageUrls base) pandoc
    adjustImageUrls base (Image attr inlines (url, title)) =
      Image attr inlines (adjustLocalUrl projectDir base url, title)
    adjustImageUrls _ inline = inline
    renderMarkdown question =
      T.unpack $
      M.substitute (chooseTemplate templates question) (MT.mFromJSON question)

compileToPandoc
  :: Y.ToJSON a
  => MT.Template -> a -> Pandoc
compileToPandoc template thing =
  case readMarkdown def $ T.unpack $ M.substitute template $ MT.mFromJSON thing of
    Left err -> throw $ PandocException (show err)
    Right pandoc -> pandoc

chooseTemplate :: MT.TemplateCache -> Question -> MT.Template
chooseTemplate templates question =
  fromJust $
  case qstAnswer question of
    MultipleAnswers _ _ -> Map.lookup "multiple-answers.md" templates
    MultipleChoice _ -> Map.lookup "multiple-choice.md" templates
    FillText _ _ -> Map.lookup "fill-text.md" templates
    FreeForm _ _ -> Map.lookup "free-form.md" templates

lookupTemplate :: String -> MT.TemplateCache -> MT.Template
lookupTemplate name templates =
  fromMaybe
    (throw $ MustacheException $ "Cannot lookup template: " ++ name)
    (Map.lookup name templates)

compileExam
  :: FilePath
  -> MT.TemplateCache
  -> (Exam, [(Student, [(Question, FilePath)])])
  -> Action Pandoc
compileExam projectDir templates (exam, students) = do
  putLoud "Compiling exam title page ..."
  let title = compileToPandoc (lookupTemplate "title-page.md" templates) exam
  putLoud "Compiling exam ..."
  list <- mapM (compileStudentExam projectDir templates exam) students
  return $ joinPandoc $ title : list

compileStudentExam
  :: FilePath
  -> MT.TemplateCache
  -> Exam
  -> (Student, [(Question, FilePath)])
  -> Action Pandoc
compileStudentExam projectDir templates exam (student, questions) = do
  putLoud "Compiling student title page ..."
  let title =
        compileToPandoc (lookupTemplate "student-title-page.md" templates) $
        StudentExam exam student
  putLoud "Compiling student questions ..."
  putLoud $ "projectDir: " ++ projectDir
  putLoud $ "Templates: " ++ show templates
  putLoud $ "Questions" ++ show questions
  list <- mapM (compileQuestion projectDir templates) questions
  putLoud "Assembling document ..."
  return $ joinPandoc $ title : list

joinPandoc :: [Pandoc] -> Pandoc
joinPandoc list = Pandoc nullMeta $ concatMap (\(Pandoc _ blocks) -> blocks) list

-- | Filters questions by LectureIds and ExcludedTopicIds.
filterQuestions :: [T.Text]
                -> [T.Text]
                -> [(Question, FilePath)]
                -> [(Question, FilePath)]
filterQuestions includeLectures excludeTopics questions =
  filter (not . (flip elem) excludeTopics . qstTopicId . fst) $
  filter ((flip elem) includeLectures . qstLectureId . fst) questions

type GroupedQuestions = Map.HashMap T.Text (Map.HashMap T.Text [(Question, FilePath)])

-- | Groups questions first by LectureId and then by TopicId into nested HashMaps.
groupQuestions :: [(Question, FilePath)] -> GroupedQuestions
groupQuestions questions =
  let byLectureId = foldl (groupBy qstLectureId) Map.empty questions
  in Map.map (foldl (groupBy qstTopicId) Map.empty) byLectureId
  where
    groupBy attrib rmap question =
      Map.insertWith (++) (attrib $ fst question) [question] rmap

-- The BUG is probably here!
generateExam
  :: Exam
  -> [(Question, FilePath)]
  -> [Student]
  -> (Exam, [(Student, [(Question, FilePath)])])
generateExam exam questions students =
  let sorted = sortOn std_employeeNumber students
      candidates =
        filterQuestions
          (examLectureIds exam)
          (examExcludedTopicIds exam)
          questions
      studentQuestions = map (selectQuestionsForStudent candidates) sorted
  in (exam, studentQuestions)
  where
    selectQuestionsForStudent :: [(Question, FilePath)]
                              -> Student
                              -> (Student, [(Question, FilePath)])
    -- | Initialize the RNG with a hash over the student data. 
    -- Should produce the identical exam for one student each time and different 
    -- exams for all students every time.
    selectQuestionsForStudent candidates student =
      let gen0 = mkStdGen (hash student)
          -- Shuffle the deck of questions
          -- shuffled = shuffle' candidates (length candidates) gen
          (shuffled, gen1) = fisherYates gen0 candidates
          -- shuffled = candidates
          -- Remove questions with duplicate TopicId. Keep the first one.
          singled =
            nubBy (\(q1, _) (q2, _) -> qstTopicId q1 == qstTopicId q2) shuffled
          -- Take the number of questions that is needed
          selection = take (examNumberOfQuestions exam) singled
          -- Shuffle multiple choices
          questions = map (shuffleAnswers gen1) selection
          -- questions = selection
          -- Number the questions 
          numbered = map numberQuestion (zip [1 ..] questions)
      in (student, numbered)
    shuffleAnswers gen (question, basePath) =
      let answer =
            case qstAnswer question of
              MultipleChoice choices
              -- MultipleChoice $ shuffle' choices (length choices) gen
               ->
                let (result, _) = fisherYates gen choices
                in MultipleChoice result
              MultipleAnswers width answers
              -- MultipleAnswers width $ shuffle' answers (length answers) gen
               ->
                let (result, _) = fisherYates gen answers
                in MultipleAnswers width result
              _ -> qstAnswer question
      in ( question
           { qstAnswer = answer
           }
         , basePath)
    numberQuestion (n, (q, p)) =
      ( q
        { qstCurrentNumber = n
        }
      , p)

-- | Throw, result is shitty.
maybeThrowYaml
  :: Y.FromJSON a
  => FilePath -> Either Y.ParseException a -> a
maybeThrowYaml _ (Right yaml) = yaml
maybeThrowYaml path (Left exception) =
  throw $
  YamlException $ "Error parsing YAML file: " ++ path ++ ", " ++ show exception

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
            path ++ ", " ++ Y.prettyPrintParseException exception
  return contents

-- Reads exam data from file
readExamData :: FilePath -> Action Exam
readExamData = readYAML

-- Reads info from all participating students.
readStudentInfo :: FilePath -> Int -> Action Students
readStudentInfo path track = do
  need [path]
  Students hashMap <- readYAML path
  let trackStudents = Map.filter (\s -> std_track s == track) hashMap
  putLoud $
    "Students in track " ++ show track ++ ": " ++ show (length trackStudents)
  return $ Students trackStudents

-- Reads all the questions and returns them along with the base directory of
-- each.
readQuestions :: [FilePath] -> Action [(Question, FilePath)]
readQuestions questions = do
  result <- mapM readQuestion questions
  putLoud $ "Questions available: " ++ show (length questions)
  return result

readQuestion :: FilePath -> Action (Question, FilePath)
readQuestion file = do
  need [file]
  result <- liftIO $ Y.decodeFileEither file
  let question =
        case result of
          Right yaml -> yaml
          Left exception ->
            throw $
            YamlException $
            "Error parsing YAML file: " ++ file ++ ", " ++ show exception
  return (question, takeDirectory file)

-- Renders a catalog of all questions (TODO sorted by LectureId and TopicId).
renderCatalog :: FilePath
              -> Templates
              -> [(Question, FilePath)]
              -> FilePath
              -> Action ()
renderCatalog projectDir templates questions out = do
  commitId <- cleanCommitIdOrFail projectDir
  let markdown = map (\(q, b) -> (renderMarkdown q, b)) questions
  let pandoc = map parseMarkdown markdown
  need $ concatMap extractLocalImagePathes pandoc
  let catalog = Pandoc nullMeta $ concatMap (\(Pandoc _ blocks) -> blocks) pandoc
  compilePandocPdf catalog out
  where
    parseMarkdown (markdown, base) =
      case readMarkdown def markdown of
        Left err -> throw $ PandocException (show err)
        Right pandoc -> walk (adjustImageUrls base) pandoc
    adjustImageUrls base (Image attr inlines (url, title)) =
      Image attr inlines (adjustLocalUrl projectDir base url, title)
    adjustImageUrls _ inline = inline
    renderMarkdown question =
      T.unpack $
      M.substitute (selectTemplate templates question) (MT.mFromJSON question)

examStationary :: Exam
examStationary =
  Exam
  { examModule = "MODULE"
  , examTitle = "TITLE"
  , examStudentInfoFile = "PATH/TO/STUDENT/INFO"
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
  , qstAnswer =
    MultipleChoice
    { answChoices =
      [ Choice "ANSWER_1" True
      , Choice "ANSWER_2" True
      , Choice "DISTRACTOR_1" False
      , Choice "DISTRACTOR_2" False
      ]
    }
  , qstDifficulty = Medium
  , qstComment = "COMMENT"
  , qstCurrentNumber = 0
  , qstBasePath = "."
  }

multipleAnswersStationary :: Question
multipleAnswersStationary =
  Question
  { qstTopicId = "TOPIC_ID"
  , qstLectureId = "LECTURE_ID"
  , qstTitle = "MULTIPLE ANSWERS"
  , qstPoints = 5
  , qstQuestion = "THE QUESTION?"
  , qstAnswer =
    MultipleAnswers
    { answWidthInMm = 30
    , answAnswers =
      [OneAnswer "DETAIL_1" "CORRECT_1", OneAnswer "DETAIL_2" "CORRECT_2"]
    }
  , qstDifficulty = Medium
  , qstComment = "COMMENT"
  , qstCurrentNumber = 0
  , qstBasePath = "."
  }

fillTextStationary :: Question
fillTextStationary =
  Question
  { qstTopicId = "TOPIC_ID"
  , qstLectureId = "LECTURE_ID"
  , qstTitle = "FILL TEXT"
  , qstPoints = 5
  , qstQuestion = "THE QUESTION?"
  , qstAnswer =
    FillText
    { answFillText = "FILL THE ___ IN THE ___."
    , answCorrectWords = ["HOLES", "TEXT"]
    }
  , qstDifficulty = Medium
  , qstComment = "COMMENT"
  , qstCurrentNumber = 0
  , qstBasePath = "."
  }

freeStationary :: Question
freeStationary =
  Question
  { qstTopicId = "TOPIC_ID"
  , qstLectureId = "LECTURE_ID"
  , qstTitle = "FREE"
  , qstPoints = 5
  , qstQuestion = "THE QUESTION?"
  , qstAnswer =
    FreeForm
    { answHeightInMm = 20
    , answCorrectAnswer = "THE ANSWER."
    }
  , qstDifficulty = Medium
  , qstComment = "COMMENT"
  , qstCurrentNumber = 0
  , qstBasePath = "."
  }
