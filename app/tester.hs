import Control.Monad ()
import Control.Exception
import Data.Maybe ()
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Yaml as Y
import Data.Yaml.Pretty as Y
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
import Filter
import Test
import Embed

main :: IO ()
main =
  do
     -- Calculate some directories
     projectDir <- calcProjectDirectory
     let privateDir = projectDir </> "private"
     -- Find sources
     testFiles <- glob "**/*-test.yaml"
     -- Meta data
     metaFiles <- glob "**/*-meta.yaml"
     -- Calculate targets
     let catalog = privateDir </> "complete-test-catalog.pdf"
     -- Prepare Mustache templates
     let templates = compileTesterTemplates
     ---
     shakeArgs shakeOptions $
       do want ["catalog"]
          --
          catalog %>
            \out ->
              do need testFiles
                 allQuestions <- readTests testFiles
                 renderCatalog projectDir templates allQuestions out
          --
          phony "catalog" $
            do need [catalog]
          --
          phony "new-mc" $
            do let string = Y.encodePretty Y.defConfig multipleChoiceStationary
               liftIO $ B.writeFile "new-mc-test.yaml" string
          --
          phony "new-ft" $
            do let string = Y.encodePretty Y.defConfig fillTextStationary
               liftIO $ B.writeFile "new-ft-test.yaml" string
          --
          phony "new-f" $
            do let string = Y.encodePretty Y.defConfig freeStationary
               liftIO $ B.writeFile "new-f-test.yaml" string
          --
          phony "clean" $
            do removeFilesAfter "." ["private"]

-- Reads all the questions and returns them along with the base directory of
-- each.
readTests :: [FilePath] -> Action [(Question, FilePath)]
readTests files = mapM readTest files
  where readTest :: FilePath -> Action (Question, FilePath)
        readTest file =
          do absolutePath <- liftIO $ makeAbsolute file
             string <- liftIO $ B.readFile absolutePath
             let question =
                   case Y.decodeEither' string of
                     Right yaml -> yaml
                     Left exception ->
                       throw $
                       YamlException $
                       "Error parsing YAML file: " ++
                       file ++ ", " ++ (show exception)
             return (question, takeDirectory absolutePath)

renderCatalog :: FilePath -> Templates -> [(Question, FilePath)] -> FilePath -> Action()
renderCatalog projectDir templates questions out =
  do let markdown = map (\(q,b) -> (renderMarkdown q,b)) questions
     let pandoc = map parseMarkdown markdown
     need $ concat $ map extractLocalImagePathes pandoc
     let catalog =
           Pandoc nullMeta $ concat $ map (\(Pandoc _ blocks) -> blocks) pandoc
     let options =
           def {writerStandalone = True
               ,writerTemplate = B.unpack testLatexTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerCiteMethod = Citeproc}
     putNormal $ "# pandoc (for " ++ out ++ ")"
     pandocMakePdf options catalog out
  where parseMarkdown (markdown,base) =
          case readMarkdown def markdown of
            Left err -> throw $ PandocException (show err)
            Right pandoc -> walk (adjustImageUrls base) pandoc
        adjustImageUrls base (Image attr inlines (url,title)) =
          (Image attr inlines (absoluteIncludePath projectDir base url,title))
        adjustImageUrls _ inline = inline
        renderMarkdown question =
          T.unpack $
          M.substitute (selectTemplate templates question)
                       (MT.mFromJSON question)

-- TODO Make this work
newOrder :: Int -> IO [Int]
newOrder n =
  do gen <- getStdGen
     return $ shuffle' [0..(n-1)] n gen

-- TODO Make this work
shuffleAnswers :: Question -> Action Question
shuffleAnswers q =
  case qstAnswer q of
    MultipleChoice choices correct ->
      do let n = length choices
         order <- liftIO $ newOrder n
         return q {qstAnswer =
                     MultipleChoice (shuffle choices order)
                                    (shuffle correct order)}
    otherwise -> return q

multipleChoiceStationary :: Question
multipleChoiceStationary =
  Question {qstId = "ID"
           ,qstLecture = 0
           ,qstTitle = "MULTIPLE CHOICE"
           ,qstPoints = 5
           ,qstQuestion = "THE QUESTION?"
           ,qstAnswer =
              MultipleChoice {answChoices = ["RIGHT_ANSWER","WRONG_ANSWER"]
                             ,answCorrectChoices = [True,False]}
           ,qstDifficulty = Medium
           ,qstComment = "COMMENT"}

fillTextStationary :: Question
fillTextStationary =
  Question {qstId = "ID"
           ,qstLecture = 0
           ,qstTitle = "FILL TEXT"
           ,qstPoints = 5
           ,qstQuestion = "THE QUESTION?"
           ,qstAnswer =
             FillText {answFillText = "FILL THE ___ IN THE ___."
                      ,answCorrectWords = ["HOLES", "TEXT"]}
           ,qstDifficulty = Medium
           ,qstComment = "COMMENT"}

freeStationary :: Question
freeStationary =
  Question {qstId = "ID"
           ,qstLecture = 0
           ,qstTitle = "FREE"
           ,qstPoints = 5
           ,qstQuestion = "THE QUESTION?"
           ,qstAnswer =
              Free {answHeightInMm = 20
                   ,answCorrectAnswer = "THE ANSWER."}
           ,qstDifficulty = Medium
           ,qstComment = "COMMENT"}
