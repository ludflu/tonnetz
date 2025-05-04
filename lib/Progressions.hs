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
readProgressions :: String -> [(Progression,Mood)]       
readProgressions pstr = map parseProgression $ splitOn '-' pstr
  where
    splitOn :: Char -> String -> [String]
    splitOn c str = case break (== c) str of
                     (x, [])    -> [x]
                     (x, _:xs)  -> x : splitOn c xs
    
    parseProgression :: String -> (Progression, Mood)
    parseProgression s = (toProgression $ map toUpper s, determineMood s)
    
    determineMood :: String -> Mood
    determineMood s = if all isUpper s then Major else Minor
    
    toProgression :: String -> Progression
    toProgression "I"    = I
    toProgression "II"   = II
    toProgression "III"  = III
    toProgression "IV"   = IV
    toProgression "V"    = V
    toProgression "VI"   = VI
    toProgression "VII"  = VII
    toProgression s      = error $ "Invalid progression symbol: " ++ s
    
