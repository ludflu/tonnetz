module ProgressionsTests where

import Data.Foldable
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Progressions
import NeoRiemann

testNote :: Note
testNote = Note C 4

testMoodDetection :: TestTree
testMoodDetection =
  testGroup
    "Mood detection in progression parsing"
    [ testCase "Detects uppercase as Major" $
        assertEqual
          "Major mood"
          Major
          (snd $ head $ readProgressions "I"),
      testCase "Detects lowercase as Minor" $
        assertEqual
          "Minor mood"
          Minor
          (snd $ head $ readProgressions "i")
    ]

testProgressionParsing :: TestTree
testProgressionParsing =
  testGroup
    "Progression parsing"
    [ testCase "Parses single numeral correctly" $
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
          (readProgressions "I-V-vi-IV")
    ]

testMakeProgression :: TestTree
testMakeProgression =
  testGroup
    "Making progression triads"
    [ testCase "Creates correct triad for I in C major" $ do
        let triad = makeMajorTriad testNote
            result = makeProgression triad I Major
        assertEqual "Root note" (Note C 4) (root result)
        assertEqual "Mood" Major (findMood result),
      testCase "Creates correct triad for V in C major" $ do
        let triad = makeMajorTriad testNote
            result = makeProgression triad V Major
        assertEqual "Root note" (Note G 4) (root result)
        assertEqual "Mood" Major (findMood result),
      testCase "Creates correct triad for vi in C major" $ do
        let triad = makeMajorTriad testNote
            result = makeProgression triad VI Minor
        assertEqual "Root note" (Note A 4) (root result)
        assertEqual "Mood" Minor (findMood result)
    ]

testMakeProgressions :: TestTree
testMakeProgressions =
  testGroup
    "Making multiple progressions"
    [ testCase "Creates a correct progression sequence" $ do
        let triad = makeMajorTriad testNote
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

progTests :: TestTree
progTests = testGroup "Progressions Tests" 
  [ testMoodDetection
  , testProgressionParsing
  , testMakeProgression
  , testMakeProgressions
  ]

