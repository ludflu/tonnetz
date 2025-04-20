module Main where

import NeoRiemann
import NeoRiemannGraph ( drawTonnetez )

import Diagrams.Prelude ( mkWidth )
import TonnetzCommands
    ( CommandArgs(..), opts )
import Options.Applicative (execParser)
import Diagrams.Backend.SVG (renderSVG)
import qualified Data.Map as M
import System.Random (initStdGen)
import FisherYates

makeTriad :: NoteClass -> Mood -> Triad
makeTriad nc m = case m of
  Major -> makeMajorTriad (Note nc 4)
  Minor -> makeMinorTriad (Note nc 4)


allTransformations :: [Transform]
allTransformations = [Leading, Parallel, Relative, Nebenverwandt, Slide, Hexapole]

run :: CommandArgs -> IO ()
run args = do
  gen <- initStdGen
  print args
  let startingTriad = makeTriad (startingKey args) (startingMood args)
      (randomTransforms, _) = fisherYates gen allTransformations
      tfs = case randomize args of 
        Just r -> take r randomTransforms
        Nothing -> transformations args
      triads = applyTransforms startingTriad tfs
      triadNames =  map show triads
      numberedTriads = M.fromList  $ zip triadNames [1..]
      tonnetz = drawTonnetez startingTriad (contextSize args) numberedTriads
   in do print tfs
         mapM_ (\t -> putStrLn $ "Triad: " ++ show t ++ " Mood: " ++ show (findMood t)) triads
         renderSVG "tonnetz.svg" (mkWidth 500) tonnetz

main :: IO ()
main = do
  args <- execParser opts
  run args
