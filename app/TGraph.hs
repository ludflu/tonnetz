{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module TGraph where

type Interval = Int

data NoteClass = C | Cs| D| Ds | E | F| Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum)

data Note = Note { noteClass :: NoteClass, octave :: Int }
    deriving (Show, Eq, Ord)

data Mood = Major | Minor
    deriving (Show, Eq)

type Triad = (Note, Note, Note)

majorThird :: Interval
majorThird = 4

minorThird :: Interval
minorThird = 3

perfectFifth :: Interval
perfectFifth = 7

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

lower :: Interval -> Note -> Note
lower = raise . negate

calcInterval :: Note -> Note -> Interval
calcInterval n1 n2 = calcSemitones n2 - calcSemitones n1

makeTriad :: Note -> Triad
makeTriad root = (root, raise majorThird root, raise perfectFifth root)


findMood :: Triad -> Mood
findMood (root, third, fifth) = 
    let thirdInterval = calcInterval root third
        fifthInterval = calcInterval root fifth
    in if thirdInterval == majorThird && fifthInterval == perfectFifth
       then Major
       else if thirdInterval == minorThird && fifthInterval == perfectFifth
            then Minor
            else error "Invalid triad"

parallel :: Triad -> Triad
parallel (root, third, fifth) = let mood = findMood (root, third, fifth)
                                in case mood of
                                    Major -> (root, lower 1 third, fifth)
                                    Minor -> (root, raise 1 third, fifth)

relative :: Triad -> Triad
relative (root, third, fifth) = let mood = findMood (root, third, fifth)
                                    in case mood of
                                        Major -> (root, third, raise 2 fifth)
                                        Minor -> (lower 2 root, third, fifth)    

leading :: Triad -> Triad
leading (root, third, fifth) = let mood = findMood (root, third, fifth)
                                        in case mood of
                                            Major -> (lower 1 root, third, fifth)
                                            Minor -> (root, third, raise 1 fifth)   

c4 :: Note
c4 = Note C 4                             

cmajor :: Triad
cmajor = makeTriad c4