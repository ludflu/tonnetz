module Main where

import Diagrams.Backend.CmdLine (Mainable(mainWith))
import NeoRiemann
import NeoRiemannGraph 

import Diagrams.Prelude
import TonnetzCommands
import Options.Applicative (execParser)

-- computeProgressions :: IO ()
-- computeProgressions =
--   do
--     gen <- initStdGen
--     let cmajor = ["A", "C", "E"]
--         (randomTransforms, _) = fisherYates gen transforms
--         path = take 4 randomTransforms
--         progression = cmajor : findChordProgression cmajor path
--     printFlat progression

makeTriad :: NoteClass -> Mood -> Triad
makeTriad nc m = case m of
  Major -> makeMajorTriad (Note nc 4)
  Minor -> makeMinorTriad (Note nc 4)

run :: CommandArgs -> IO ()
run args = do
  print args
  let startingTriad = makeTriad (startingKey args) (startingMood args)
  mainWith $ drawTriad startingTriad


main :: IO ()
main = do
  args <- execParser opts
  run args

-- c4 :: Note
-- c4 = Note C 4                             

-- cmajor :: Triad
-- cmajor = makeMajorTriad c4

-- aminor :: Triad
-- aminor = makeMinorTriad (Note A 4)

-- main :: IO ()
-- -- main = mainWith $ drawTriad cmajor
-- main = mainWith $ drawTonnetez cmajor 
