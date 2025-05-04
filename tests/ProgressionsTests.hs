module ProgressionsTests where

import Data.Foldable
import Data.List (isPrefixOf, tails)
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
    [ testCase "Detects uppercase as Major" $ do
        case readProgressions "I" of
          Right progs -> assertEqual
                           "Major mood"
                           Major
                           (snd $ head progs)
          Left err -> assertBool err False,
      testCase "Detects lowercase as Minor" $ do
        case readProgressions "i" of
          Right progs -> assertEqual
                           "Minor mood"
                           Minor
                           (snd $ head progs)
          Left err -> assertBool err False
    ]

testProgressionParsing :: TestTree
testProgressionParsing =
  testGroup
    "Progression parsing"
    [ testCase "Parses single numeral correctly" $ do
        case readProgressions "IV" of
          Right progs -> assertEqual
                           "IV progression"
                           IV
                           (fst $ head progs)
          Left err -> assertBool err False,
      testCase "Parses single lowercase numeral correctly" $ do
        case readProgressions "vi" of
          Right progs -> assertEqual
                           "VI progression"
                           VI
                           (fst $ head progs)
          Left err -> assertBool err False,
      testCase "Parses multiple progressions correctly" $ do
        case readProgressions "I-V-vi-IV" of
          Right progs -> assertEqual
                           "I-V-vi-IV chord progression"
                           [(I, Major), (V, Major), (VI, Minor), (IV, Major)]
                           progs
          Left err -> assertBool err False,
      testCase "Returns error for invalid progression" $
        let isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
        in do
          case readProgressions "I-X-vi-IV" of
            Right _ -> assertBool "Expected error for invalid progression" False
            Left err -> assertBool "Error contains invalid symbol message" 
                        ("Invalid progression symbol" `isInfixOf` err && "X" `isInfixOf` err)
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
        case readProgressions "I-V-vi-IV" of
          Right prog -> do
            let result = makeProgressions triad prog
            assertEqual "Sequence length" 4 (length result)
            assertEqual "Root notes" 
                      [Note C 4, Note G 4, Note A 4, Note F 4] 
                      (map root result)
            assertEqual "Moods"
                      [Major, Major, Minor, Major]
                      (map findMood result)
          Left err -> assertBool err False
    ]

progTests :: TestTree
progTests = testGroup "Progressions Tests" 
  [ testMoodDetection
  , testProgressionParsing
  , testMakeProgression
  , testMakeProgressions
  ]

