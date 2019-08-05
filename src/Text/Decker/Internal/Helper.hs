module Text.Decker.Internal.Helper
  ( dropSuffix
  , replaceSuffix
  , repeatIfTrue
  , whenTrue
  , unique
  , time
  , (<++>)
  , runIOQuietly
  ) where

import Control.Monad.State
import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Set as Set
import System.CPUTime
import Text.Pandoc
import Text.Printf

runIOQuietly :: PandocIO a -> IO (Either PandocError a)
runIOQuietly act = runIO (setVerbosity ERROR >> act)

-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

repeatIfTrue :: Monad m => m Bool -> m ()
repeatIfTrue action = do
  again <- action
  when again $ repeatIfTrue action

whenTrue :: Monad m => m Bool -> m () -> m ()
whenTrue bool action = do
  true <- bool
  when true action

-- | Removes the last suffix from a filename
dropSuffix :: String -> String -> String
dropSuffix s t = fromMaybe t (List.stripSuffix s t)

replaceSuffix :: String -> String -> String -> String
replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

time :: String -> IO a -> IO a
time name action = do
  start <- getCPUTime
  result <- action
  stop <- getCPUTime
  let diff = fromIntegral (stop - start) / (10 ^ 12)
  printf "%s: %0.5f sec\n" name (diff :: Double)
  return result
