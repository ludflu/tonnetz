module Main where

import Diagrams.Backend.CmdLine (Mainable(mainWith))
import NeoRiemann
import NeoRiemannGraph 

import Diagrams.Prelude
import TonnetzCommands
import Options.Applicative (execParser)
import Diagrams.Backend.SVG (renderSVG)

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
      triads = applyTransforms startingTriad (transformations args)
      -- Print each triad along with its mood
      -- Still render the tonnetz based on the starting triad
      tonnetz = drawTonnetez startingTriad
   in do _ <- mapM (\t -> putStrLn $ "Triad: " ++ show t ++ " Mood: " ++ show (findMood t)) triads
         renderSVG "tonnetz.svg" (mkWidth 500) tonnetz

main :: IO ()
main = do
  args <- execParser opts
  run args
