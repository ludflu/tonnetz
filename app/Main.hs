module Main where

import FisherYates
  ( fisherYates,
  )
import System.Random
  ( initStdGen)
import TonnetzCommands
import Options.Applicative (execParser)
import ChordGraph (transforms, printFlat, findChordProgression)
import NeoRiemannGraph (drawTonnetz)
import Diagrams.Backend.CmdLine (Mainable(mainWith))
import NeoRiemann (cmajor)

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
  mainWith $ drawTonnetz cmajor


-- main :: IO ()
-- main = do
--   args <- execParser opts
--   run args

main :: IO ()
main = mainWith $ drawTonnetz cmajor