module Main where

import ChordGraph
import FisherYates
  ( fisherYates,
  )
import System.Random
  ( StdGen,
    initStdGen,
  )
  
main :: IO ()
main =
  do
    gen <- initStdGen
    let cmajor = ["C", "E", "G"]
        tfs = [lp, pl, pr, pl]
        (randomTransforms, _) = fisherYates gen transforms
        (randomTransformNames, _) = fisherYates gen transformNames
        path = take 4 randomTransforms
        progression = cmajor : findChordProgression cmajor path
    printFlat progression
