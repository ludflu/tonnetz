{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TGraph where

-- import Data.Modular (ℤ, type (/), toMod)

type Interval = Int

data NoteClass = C | Cs| D| Ds | E | F| Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum)

data Note = Note { noteClass :: NoteClass, octave :: Int }

calcSemitones :: Note -> Interval
calcSemitones n =  let o = octave n
                       nc = noteClass n
                    in fromEnum nc + 12 * o

raise :: Interval -> Note -> Note
raise i n = let o = octave n
                nc = noteClass n
             in Note (toEnum ((fromEnum nc + i) `mod` 12)) (o + (fromEnum nc + i) `div` 12)


calcInterval :: Note -> Note -> Interval
calcInterval n1 n2 = calcSemitones n2 - calcSemitones n1

