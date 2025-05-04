module Main where

-- import ChordGraph (TriadicTransform (Leading, Parallel, Relative), triadGraph)
import Data.Foldable
import Data.List.Unique (sortUniq)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import NeoRiemann
import Progressions


-- a node is good if it has exactly three neighbors and the edges are labeled correctly
-- goodNode :: G.Node -> Bool
-- goodNode n =
--   let es = G.out triadGraph n
--       lbls = sortUniq $ map third es
--    in length es == 3 && lbls == [Leading, Relative, Parallel]

-- allGoodNodes :: [G.Node] -> Bool
-- allGoodNodes ns =
--   let ag = map goodNode ns
--    in and ag

progressionsUnitTests :: TestTree
progressionsUnitTests =
  testGroup
    "progressions tests"
    [ testCase "Parses uppercase as Major" $
        assertEqual
          "Major mood"
          Major
          (snd $ head $ readProgressions "I"),
      testCase "Parses lowercase as Minor" $
        assertEqual
          "Minor mood"
          Minor
          (snd $ head $ readProgressions "i"),
      testCase "Parses single numeral correctly" $
        assertEqual
          "IV progression"
          IV
          (fst $ head $ readProgressions "IV"),
      testCase "Parses single lowercase numeral correctly" $
        assertEqual
          "VI progression"
          VI
          (fst $ head $ readProgressions "vi"),
      testCase "Parses multiple progressions correctly" $
        assertEqual
          "I-V-vi-IV chord progression"
          [(I, Major), (V, Major), (VI, Minor), (IV, Major)]
          (readProgressions "I-V-vi-IV"),
      testCase "Makes correct progression for I in C major" $ do
        let testNote = Note C 4
            triad = makeMajorTriad testNote
            result = makeProgression triad I Major
        assertEqual "Root note" (Note C 4) (root result)
        assertEqual "Mood" Major (findMood result),
      testCase "Makes correct progression for V in C major" $ do
        let testNote = Note C 4
            triad = makeMajorTriad testNote
            result = makeProgression triad V Major
        assertEqual "Root note" (Note G 4) (root result)
        assertEqual "Mood" Major (findMood result),
      testCase "Makes correct progression for vi in C major" $ do
        let testNote = Note C 4
            triad = makeMajorTriad testNote
            result = makeProgression triad VI Major
        assertEqual "Root note" (Note A 4) (root result)
        assertEqual "Mood" Minor (findMood result),
      testCase "Makes correct progression sequence" $ do
        let testNote = Note C 4
            triad = makeMajorTriad testNote
            prog = readProgressions "I-V-vi-IV"
            result = makeProgressions triad prog
        assertEqual "Sequence length" 4 (length result)
        assertEqual "Root notes" 
                   [Note C 4, Note G 4, Note A 4, Note F 4] 
                   (map root result)
        assertEqual "Moods"
                   [Major, Major, Minor, Major]
                   (map findMood result)
    ]

-- tonnetzUnitTests :: TestTree
-- tonnetzUnitTests =
--   testGroup
--     "tonnetz tests"
--     [ testCase "All good nodes" $
--         assertEqual
--           "all good"
--           True
--           ( allGoodNodes
--               chordNodes
--           ),
--       testCase
--         "number of nodes"
--         $ assertEqual "num nodes" (length chordNodes) 24
--     ]

allTests :: TestTree
allTests = testGroup "All Tests" [progressionsUnitTests]

main :: IO ()
main = defaultMain allTests
