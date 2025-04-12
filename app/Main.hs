module Main where

import ChordGraph
import FisherYates
  ( fisherYates,
  )
import System.Random
  ( StdGen,
    initStdGen,
  )
import TonnetzGraph qualified as TG
import TonnetzCommands
import Options.Applicative (execParser)

computeProgressions :: IO ()
computeProgressions =
  do
    gen <- initStdGen
    let cmajor = ["C", "E", "G"]
        tfs = [lp, pl, pr, pl]
        (randomTransforms, _) = fisherYates gen transforms
        (randomTransformNames, _) = fisherYates gen transformNames
        path = take 4 randomTransforms
        progression = cmajor : findChordProgression cmajor path
    printFlat progression

run :: CommandArgs -> IO ()
run args = do
  print args

main :: IO ()
main = do
  args <- execParser opts
  run args

-- main = TG.main