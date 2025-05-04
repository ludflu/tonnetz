module ProgressionsTests where

import Test.Hspec
import NeoRiemann
import Progressions

testNote :: Note
testNote = Note C 4

testMoodDetection :: Spec
testMoodDetection = describe "Mood detection in progression parsing" $ do
  it "detects uppercase as Major" $ do
    let result = snd $ head $ readProgressions "I"
    result `shouldBe` Major
  
  it "detects lowercase as Minor" $ do
    let result = snd $ head $ readProgressions "i"
    result `shouldBe` Minor

testProgressionParsing :: Spec
testProgressionParsing = describe "Progression parsing" $ do
  it "parses single numeral correctly" $ do
    let result = fst $ head $ readProgressions "IV"
    result `shouldBe` IV
  
  it "parses single lowercase numeral correctly" $ do
    let result = fst $ head $ readProgressions "vi"
    result `shouldBe` VI
  
  it "parses multiple progressions correctly" $ do
    let result = readProgressions "I-V-vi-IV"
    result `shouldBe` [(I, Major), (V, Major), (VI, Minor), (IV, Major)]

testMakeProgression :: Spec
testMakeProgression = describe "Making progression triads" $ do
  it "creates correct triad for I in C major" $ do
    let triad = makeMajorTriad testNote
        result = makeProgression triad I Major
    root result `shouldBe` Note C 4
    findMood result `shouldBe` Major
  
  it "creates correct triad for V in C major" $ do
    let triad = makeMajorTriad testNote
        result = makeProgression triad V Major
    root result `shouldBe` Note G 4
    findMood result `shouldBe` Major
  
  it "creates correct triad for vi in C major" $ do
    let triad = makeMajorTriad testNote
        result = makeProgression triad VI Major
    root result `shouldBe` Note A 4
    findMood result `shouldBe` Minor

testMakeProgressions :: Spec
testMakeProgressions = describe "Making multiple progressions" $ do
  it "creates a correct progression sequence" $ do
    let triad = makeMajorTriad testNote
        prog = readProgressions "I-V-vi-IV"
        result = makeProgressions triad prog
    length result `shouldBe` 4
    map root result `shouldBe` [Note C 4, Note G 4, Note A 4, Note F 4]
    map findMood result `shouldBe` [Major, Major, Minor, Major]

spec :: Spec
spec = do
  testMoodDetection
  testProgressionParsing
  testMakeProgression
  testMakeProgressions

main :: IO ()
main = hspec spec