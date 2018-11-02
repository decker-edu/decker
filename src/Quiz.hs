{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> --}
module Quiz
  ( renderQuizzes,
  dachdeckerUrl
  ) where

import Common
import Text.Pandoc
import Text.Pandoc.Walk
import System.Environment

renderQuizzes :: Pandoc -> Decker Pandoc
renderQuizzes pandoc = do
  return $ walk renderQuiz pandoc

-- | Renders a quiz
-- A quiz is a bullet list in the style of a task list.
-- A div class survey is created around the bullet list
renderQuiz :: Block -> Block
-- BulletList which qualifies as survey
renderQuiz (BulletList blocks@((firstBlock:_):_))
  | checkIfQuiz firstBlock =
    Div ("", ["survey"], []) [BulletList (map renderAnswer blocks)]
-- Default pass through
renderQuiz block = block

-- | Checks if a block starts with [X] or [ ] to indicate a survey
checkIfQuiz :: Block -> Bool
checkIfQuiz (Para ((Str "[X]"):_)) = True
checkIfQuiz (Para ((Str "["):Space:(Str "]"):_)) = True
checkIfQuiz (Plain ((Str "[X]"):_)) = True
checkIfQuiz (Plain ((Str "["):Space:(Str "]"):_)) = True
checkIfQuiz _ = False

-- | Renders a quiz answer 
-- Throws away the identifier and sourrounds the content with a div
-- The div has the class right or wrong according to how it was marked
renderAnswer :: [Block] -> [Block]
renderAnswer (prelude:rest) =
  [Div ("", "answer" : cls, []) (prelude' : (map renderTooltip rest))]
  where
    (cls, prelude') =
      case prelude of
        Para ((Str "[X]"):prest) -> (["right"], Para prest)
        Para ((Str "["):Space:(Str "]"):prest) -> (["wrong"], Para prest)
        Plain ((Str "[X]"):prest) -> (["right"], Para prest)
        Plain ((Str "["):Space:(Str "]"):prest) -> (["wrong"], Para prest)
        prest -> ([], prest)

-- if there is a bullet list create a div class tooltip around
-- if there are multiple bullet points, all but the first are thrown away
renderTooltip :: Block -> Block
renderTooltip (BulletList (content:_)) = Div ("", ["tooltip"], []) content
renderTooltip block = block

dachdeckerUrl :: IO String
dachdeckerUrl = do
  env <- System.Environment.lookupEnv "DACHDECKER_SERVER"
  let url = case env of
        Just val -> val
        Nothing -> "https://dach.decker.informatik.uni-wuerzburg.de"
  return url