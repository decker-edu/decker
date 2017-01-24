module Shuffle
  ( fisherYates
  ) where

import System.Random
import Data.Map as M

fisherYatesStep
  :: RandomGen g
  => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates
  :: RandomGen g
  => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $
  Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1 ..]
    initial x gen = (singleton 0 x, gen)
