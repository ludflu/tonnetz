module FisherYates where

import qualified Data.Map as Map
import System.Random
  ( RandomGen,
    randomR,
  )

fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1 ..]
    initial x gen = (Map.singleton 0 x, gen)
