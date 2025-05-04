module Progressions where

import NeoRiemann
import Data.Char (toUpper, isUpper)

data Progression = I | II | III | IV | V | VI | VII deriving (Enum, Eq, Show)



makeProgression :: Triad -> Progression -> Mood -> Triad
makeProgression triad@(Triad r _ _ _) progression mood = let scalePattern = case findMood triad of
                                                                Minor -> minor
                                                                Major -> major
                                                             scale = makeScale scalePattern r
                                                             indx = fromEnum progression
                                                             newRoot = scale !! indx
                                                             newTriad = case mood of 
                                                                Major -> makeMajorTriad newRoot
                                                                Minor -> makeMinorTriad newRoot
                                                          in newTriad

makeProgressions :: Triad -> [(Progression,Mood)] -> [Triad]
makeProgressions t pgs = let progTxf = uncurry $ makeProgression t
                          in map progTxf pgs

--takes a string of the form "I-V-vi-IV" and parses it into tuples of (Progression,Mood)
-- if the roman numeral symbol is lower case, we make it minor, if upper case, then Major
-- Returns Either String [(Progression,Mood)] where Left contains error message on parse failure
readProgressions :: String -> Either String [(Progression,Mood)]       
readProgressions pstr = traverse parseProgression $ splitOn '-' pstr
  where
    splitOn :: Char -> String -> [String]
    splitOn c str = case break (== c) str of
                     (x, [])    -> [x]
                     (x, _:xs)  -> x : splitOn c xs
    
    parseProgression :: String -> Either String (Progression, Mood)
    parseProgression s = case toProgression $ map toUpper s of
                           Right prog -> Right (prog, determineMood s)
                           Left err   -> Left err
    
    determineMood :: String -> Mood
    determineMood s = if all isUpper s then Major else Minor
    
    toProgression :: String -> Either String Progression
    toProgression "I"    = Right I
    toProgression "II"   = Right II
    toProgression "III"  = Right III
    toProgression "IV"   = Right IV
    toProgression "V"    = Right V
    toProgression "VI"   = Right VI
    toProgression "VII"  = Right VII
    toProgression s      = Left $ "Invalid progression symbol: " ++ s
    
