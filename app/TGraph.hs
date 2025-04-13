{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TGraph where

-- import Data.Modular (ℤ, type (/), toMod)

type Interval = Int

data NoteClass = C | Cs| D| Ds | E | F| Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum)

data Note = Note { noteClass :: NoteClass, octave :: Int }
    deriving (Show, Eq, Ord)

calcSemitones :: Note -> Interval
calcSemitones n =  let o = octave n
                       nc = noteClass n
                    in fromEnum nc + 12 * o

raise :: Interval -> Note -> Note
raise i n = let 
                translatedNoteIdx = fromEnum (noteClass n) + i
                translatedNote = toEnum (translatedNoteIdx `mod` 12)
                translatedOctave = octave n + translatedNoteIdx `div` 12
            in Note translatedNote translatedOctave


calcInterval :: Note -> Note -> Interval
calcInterval n1 n2 = calcSemitones n2 - calcSemitones n1

