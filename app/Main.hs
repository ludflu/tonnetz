module Main where

import NeoRiemann
    ( findMood,
      makeMajorTriad,
      makeMinorTriad,
      Mood(..),
      Note(Note),
      NoteClass,
      Triad,
      applyTransforms )
import NeoRiemannGraph ( drawTonnetez )

import Diagrams.Prelude ( mkWidth )
import TonnetzCommands
    ( CommandArgs(transformations, startingKey, startingMood), opts )
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
   in do mapM_ (\t -> putStrLn $ "Triad: " ++ show t ++ " Mood: " ++ show (findMood t)) triads
         renderSVG "tonnetz.svg" (mkWidth 500) tonnetz

main :: IO ()
main = do
  args <- execParser opts
  run args
