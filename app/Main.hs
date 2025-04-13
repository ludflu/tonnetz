module Main where

import FisherYates
  ( fisherYates,
  )
import System.Random
  ( initStdGen)
import TonnetzCommands ( CommandArgs, opts )
import Options.Applicative (execParser)
import ChordGraph (transforms, printFlat, findChordProgression)

computeProgressions :: IO ()
computeProgressions =
  do
    gen <- initStdGen
    let cmajor = ["C", "E", "G"]
        (randomTransforms, _) = fisherYates gen transforms
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

-- main :: IO ()
-- main = mainWith drawC