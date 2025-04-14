module Main where

import Diagrams.Backend.CmdLine (Mainable(mainWith))
import NeoRiemann
import NeoRiemannGraph 

import Diagrams.Prelude
-- computeProgressions :: IO ()
-- computeProgressions =
--   do
--     gen <- initStdGen
--     let cmajor = ["A", "C", "E"]
--         (randomTransforms, _) = fisherYates gen transforms
--         path = take 4 randomTransforms
--         progression = cmajor : findChordProgression cmajor path
--     printFlat progression

-- run :: CommandArgs -> IO ()
-- run args = do
--   print args
--   mainWith $ drawMinorTriad aminor


-- main :: IO ()
-- main = do
--   args <- execParser opts
--   run args

main :: IO ()
-- main = mainWith $ drawTriad cmajor
main = mainWith $ drawTonnetez cmajor 
