module NeoRiemannTests where

import Data.Foldable
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import NeoRiemann

testCMajor :: Triad
testCMajor = makeMajorTriad (Note C 4)

testAMinor :: Triad
testAMinor = makeMinorTriad (Note A 4)

testNoteOperations :: TestTree
testNoteOperations =
  testGroup
    "Note operations"
    [ testCase "Raising a note by interval" $ do
        let n = Note C 4
            raised = raise n majorThird
        assertEqual "C raised by a major third should be E" (Note E 4) raised,
      testCase "Lowering a note by interval" $ do
        let n = Note E 4
            lowered = lower n majorThird
        assertEqual "E lowered by a major third should be C" (Note C 4) lowered,
      testCase "Computing interval between notes" $ do
        let n1 = Note C 4
            n2 = Note G 4
        assertEqual "Interval between C and G should be a perfect fifth" 
                   perfectFifth
                   (calcInterval n1 n2)
    ]

testTriadOperations :: TestTree
testTriadOperations =
  testGroup
    "Triad operations"
    [ testCase "Creating a major triad" $ do
        let triad = makeMajorTriad (Note C 4)
        assertEqual "Root note" (Note C 4) (root triad)
        assertEqual "Third note" (Note E 4) (third triad)
        assertEqual "Fifth note" (Note G 4) (fifth triad)
        assertEqual "Mood" Major (findMood triad),
      testCase "Creating a minor triad" $ do
        let triad = makeMinorTriad (Note A 4)
        assertEqual "Root note" (Note A 4) (root triad)
        assertEqual "Third note" (Note C 5) (third triad)
        assertEqual "Fifth note" (Note E 5) (fifth triad)
        assertEqual "Mood" Minor (findMood triad)
    ]

testTransformations :: TestTree
testTransformations =
  testGroup
    "Neo-Riemannian transformations"
    [ testCase "Parallel transformation (major to minor)" $ do
        let transformed = parallel testCMajor
        assertEqual "Root should remain the same" (Note C 4) (root transformed)
        assertEqual "Third should be lowered by half step" (Note Ds 4) (third transformed)
        assertEqual "Fifth should remain the same" (Note G 4) (fifth transformed)
        assertEqual "Mood should change to minor" Minor (findMood transformed),
      testCase "Parallel transformation (minor to major)" $ do
        let transformed = parallel testAMinor
        assertEqual "Root should remain the same" (Note A 4) (root transformed)
        assertEqual "Third should be raised by half step" (Note Cs 5) (third transformed)
        assertEqual "Fifth should remain the same" (Note E 5) (fifth transformed)
        assertEqual "Mood should change to major" Major (findMood transformed),
      testCase "Relative transformation (major to minor)" $ do
        let transformed = relative testCMajor
        assertEqual "New root should be A" (Note A 4) (root transformed)
        assertEqual "New third should be C" (Note C 4) (third transformed)
        assertEqual "New fifth should be E" (Note E 4) (fifth transformed)
        assertEqual "Mood should change to minor" Minor (findMood transformed),
      testCase "Relative transformation (minor to major)" $ do
        let transformed = relative testAMinor
        assertEqual "New root should be C" (Note C 5) (root transformed)
        assertEqual "New third should be E" (Note E 5) (third transformed)
        assertEqual "New fifth should be G" (Note G 4) (fifth transformed)
        assertEqual "Mood should change to major" Major (findMood transformed),
      testCase "Leading tone exchange (major to minor)" $ do
        let transformed = leading testCMajor
        assertEqual "New root should be E" (Note E 4) (root transformed)
        assertEqual "New third should be G" (Note G 4) (third transformed)
        assertEqual "New fifth should be B" (Note B 3) (fifth transformed)
        assertEqual "Mood should change to minor" Minor (findMood transformed),
      testCase "Leading tone exchange (minor to major)" $ do
        let transformed = leading testAMinor
        assertEqual "New root should be F" (Note F 5) (root transformed)
        assertEqual "New third should be A" (Note A 4) (third transformed)
        assertEqual "New fifth should be C" (Note C 5) (fifth transformed)
        assertEqual "Mood should change to major" Major (findMood transformed)
    ]

testCompositeTransforms :: TestTree
testCompositeTransforms =
  testGroup
    "Composite transformations"
    [ testCase "Slide transformation" $ do
        let transformed = slide testCMajor
        assertEqual "Slide should be L-P-R" (Note Cs 4) (root transformed)
        assertEqual "Mood should change" Minor (findMood transformed),
      testCase "Nebenverwandt transformation" $ do
        let transformed = nebenverwandt testCMajor
        assertEqual "Nebenverwandt should be R-L-P" (Note F 4) (root transformed)
        assertEqual "Mood should be Minor" Minor (findMood transformed),
      testCase "Hexapole transformation" $ do
        let transformed = hexapole testCMajor
        assertEqual "Hexapole should be L-P-L" (Note Gs 4) (root transformed)
        assertEqual "Mood should change" Minor (findMood transformed)
    ]

testScales :: TestTree
testScales =
  testGroup
    "Scale generation"
    [ testCase "Major scale generation" $ do
        let scale = makeScale major (Note C 4)
        assertEqual "C major scale first note" (Note C 4) (head scale)
        assertEqual "C major scale length" 8 (length scale)
        assertEqual "C major scale notes" 
                   [Note C 4, Note D 4, Note E 4, Note F 4, 
                    Note G 4, Note A 4, Note B 4, Note C 5]
                   scale,
      testCase "Minor scale generation" $ do
        let scale = makeScale minor (Note A 4)
        assertEqual "A minor scale first note" (Note A 4) (head scale)
        assertEqual "A minor scale length" 8 (length scale)
        assertEqual "A minor scale notes" 
                   [Note A 4, Note B 4, Note C 5, Note D 5, 
                    Note E 5, Note F 5, Note G 5, Note A 5]
                   scale
    ]

neoRiemannTests :: TestTree
neoRiemannTests = testGroup "NeoRiemann Tests" 
  [ testNoteOperations
  , testTriadOperations
  , testTransformations
  , testCompositeTransforms
  , testScales
  ]