module Main where

import NeoRiemann
import NeoRiemannGraph ( drawTonnetez )

import Diagrams.Prelude ( mkWidth )
import TonnetzCommands
    ( CommandArgs(..), opts )
import Options.Applicative (execParser)
import Diagrams.Backend.SVG (renderSVG)
import qualified Data.Map as M
import System.Random
import FisherYates
import Control.Monad (unless, when)
import NotesToEuterpea (playTriads)
import Data.Maybe (fromMaybe)

makeTriad :: NoteClass -> Mood -> Triad
makeTriad nc m = case m of
  Major -> makeMajorTriad (Note nc 4)
  Minor -> makeMinorTriad (Note nc 4)



run :: CommandArgs -> IO ()
run args = do
  gen <- getStdGen
  let startingTriad = makeTriad (startingKey args) (startingMood args)
      transforms = fromMaybe allTransformations (transformations args)
      (randomTransforms, _) = fisherYates gen transforms
      tfs = case randomize args of 
        Just r -> take r randomTransforms
        Nothing -> transforms
      triads = applyTransforms startingTriad tfs
      transformedTriads = zip (startingTriad : triads) tfs
      triadNames =  map (show . cleanCrumbs) triads
      numberedTriads = M.fromList  $ zip triadNames [1..]

      tonnetz = drawTonnetez startingTriad transformedTriads (contextSize args) M.empty --numberedTriads
   in do when (verbose args) (print args )
         when (verbose args) (print tfs )
         playTriads triads (duration args)
         mapM_ (\t -> do
                   putStrLn $ "Triad: " ++ show t ++ " Mood: " ++ show (findMood t)
                   let Triad _ _ _ crumbs = t
                   unless (null crumbs) $ putStrLn $ "  Breadcrumbs: " ++ show (reverse crumbs)) triads
         renderSVG "tonnetz.svg" (mkWidth 500) tonnetz

main :: IO ()
main = do
  args <- execParser opts
  run args
