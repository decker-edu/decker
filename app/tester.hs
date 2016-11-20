{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ()
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
                      Students studentMap <- readStudentInfo studentInfoPath (examTrack exam)
                      putNormal $ "Read student data from: " ++ studentInfoPath
                      let examData = generateExam exam questions (Map.elems studentMap)
                      putNormal $
                          "Exams generated for N students. N: " ++ (show . length . snd) examData
                      templates <- compileTemplates "exam"
                      putNormal "Templates compiled."
                      examPandoc <- compileExam projectDir templates examData
                      putNormal "Exams compiled to pandoc"
                      compilePandocPdf examPandoc out
              --
              phony
                  "clean" $
                  removeFilesAfter "." ["private"]

compileTemplates :: FilePath -> Action MT.TemplateCache
compileTemplates disposition = do
    let templateNames = 
            [ "title-page.md"
            , "student-title-page.md"
            , "multiple-choice.md"
            , "fill-text.md"
            , "free-form.md"]
    compiled <- mapM (compileProjectTemplate disposition) templateNames
    return $ Map.fromList $ zip templateNames compiled

compileProjectTemplate :: FilePath -> FilePath -> Action MT.Template
compileProjectTemplate disposition name = do
    projectDir <- getProjectDir
    let filename = projectDir </> "exams" </> "templates" </> disposition </> name
    need [filename]
    text <- liftIO $ T.readFile $ filename
    let result = M.compileTemplate name (fixMustacheMarkupText text)
    return $
        case result of
            Right templ -> templ
            Left parseError -> 
                throw $
                MustacheException $ "Error parsing mustache template: " ++ (show parseError)

compilePandocPdf :: Pandoc -> FilePath -> Action ()
compilePandocPdf exam out = do
    let variables = 
            [ ("fontsize", "12pt")
            , ("fontfamily", "roboto")
            , ("header-includes", "\\renewcommand{\\familydefault}{\\sfdefault}")]
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

compileQuestion :: FilePath -> MT.TemplateCache -> (Question, FilePath) -> Action Pandoc
compileQuestion projectDir templates question = do
    return $ compileToPandoc question
  where
    compileToPandoc :: (Question, FilePath) -> Pandoc
    compileToPandoc (quest,base) = 
        case readMarkdown def (renderMarkdown quest) of
            Left err -> throw $ PandocException (show err)
            Right pandoc -> walk (adjustImageUrls base) pandoc
    adjustImageUrls base (Image attr inlines (url,title)) = 
        (Image attr inlines (adjustLocalUrl projectDir base url, title))
    adjustImageUrls _ inline = inline
    renderMarkdown question = 
        T.unpack $ M.substitute (chooseTemplate templates question) (MT.mFromJSON question)

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
        MultipleChoice _ -> Map.lookup "multiple-choice.md" templates
        FillText _ _ -> Map.lookup "fill-text.md" templates
        FreeForm _ _ -> Map.lookup "free-form.md" templates

lookupTemplate :: String -> MT.TemplateCache -> MT.Template
lookupTemplate name templates = 
    case Map.lookup name templates of
        Just t -> t
        Nothing -> throw $ MustacheException $ "Cannot lookup template: " ++ name

compileExam :: FilePath
            -> MT.TemplateCache
            -> (Exam, [(Student, [(Question, FilePath)])])
            -> Action Pandoc
compileExam projectDir templates (exam,students) = do
    let title = compileToPandoc (lookupTemplate "title-page.md" templates) exam
    list <- mapM (compileStudentExam projectDir templates exam) students
    return $ joinPandoc $ title : list

compileStudentExam :: FilePath
                   -> MT.TemplateCache
                   -> Exam
                   -> (Student, [(Question, FilePath)])
                   -> Action Pandoc
compileStudentExam projectDir templates exam (student,questions) = do
    let title = 
            compileToPandoc (lookupTemplate "student-title-page.md" templates) $
            StudentExam exam student
    list <- mapM (compileQuestion projectDir templates) questions
    return $ joinPandoc $ title : list

{-
compileExam :: (Exam, [(Student, [(Question, FilePath)])]) -> Action T.Text
compileExam exam = do
    projectDir <- getProjectDir
    let templateFile = "exam-template.md"
    result <- liftIO $ M.automaticCompile [projectDir </> "test" </> "exams"] templateFile
    let template = 
            case result of
                Right templ -> templ
                Left parseError -> 
                    throw $
                    MustacheException $ "Error parsing mustache template: " ++ (show parseError)
    return $ M.substitute template (MT.mFromJSON exam)
-}
{-
renderExam:: (Exam, [(Student, [Question])]) -> FilePath -> Action ()
renderExam exam pdfPath = do
    titlePage <- renderTitlePage (fst exam) (length $ snd exam)
    studentExams <- mapM (renderStudentExams $ fst exam) $ snd exam
renderExam exam pdfPath = do
    frontPage = renderFrontPage exam 
-}
joinPandoc
    :: [Pandoc] -> Pandoc
joinPandoc list = 
    Pandoc nullMeta $
    concatMap
        (\(Pandoc _ blocks) -> 
              blocks)
        list

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

generateExam :: Exam
             -> [(Question, FilePath)]
             -> [Student]
             -> (Exam, [(Student, [(Question, FilePath)])])
generateExam exam questions students = 
    let sorted = sortOn std_employeeNumber students
        candidates = filterQuestions (examLectureIds exam) (examExcludedTopicIds exam) questions
        studentQuestions = map (selectQuestionsForStudent candidates) sorted
    in (exam, studentQuestions)
  where
    selectQuestionsForStudent :: [(Question, FilePath)]
                              -> Student
                              -> (Student, [(Question, FilePath)])
    selectQuestionsForStudent candidates student = 
        -- | Initialize the RNG with a hash over the student data. 
        -- Should produce the identical exam for one student each time and different 
        -- exams for all students every time.
        let gen = mkStdGen (hash student)
            selection = 
                take (examNumberOfQuestions exam) $ shuffle' candidates (length candidates) gen
            questions = map (shuffleChoices gen) selection
        in (student, questions)
    shuffleChoices gen (question,basePath) = 
        let answer = 
                case qstAnswer question of
                    MultipleChoice choices -> 
                        MultipleChoice $ shuffle' choices (length choices) gen
                    _ -> qstAnswer question
        in ( question
             { qstAnswer = answer
             }
           , basePath)

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
    let trackStudents = 
            Map.filter
                (\s -> 
                      std_track s == track)
                hashMap
    putNormal $ "Students in track " ++ show track ++ ": " ++ show (length trackStudents)
    return $ Students trackStudents

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