{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NeoRiemann where

import Control.Arrow ( (>>>) )

type Interval = Int

data Transform = Leading | Parallel | Relative | Slide | Nebenverwandt | Hexapole
  deriving (Show, Eq)

data NoteClass = C | Cs| D| Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Ord, Enum)

type Es = F
type Bs = C

data Note = Note { noteClass :: NoteClass, octave :: Int }
    deriving (Eq, Ord)

instance Show Note where
    show (Note nc oct) = show nc ++ show oct

data Mood = Major | Minor
    deriving (Show, Eq)

type Triad = (Note, Note, Note)

instance Show NoteClass where
    show :: NoteClass -> String
    show C  = "C"
    show Cs = "C#"
    show D  = "D"
    show Ds = "D#"
    show E  = "E"
    show F  = "F"
    show Fs = "F#"
    show G  = "G"
    show Gs = "G#"
    show A  = "A"
    show As = "A#"
    show B  = "B"

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
calcInterval n1 n2 = let i = calcSemitones n2 - calcSemitones n1
                      in i `mod` 12

makeMajorTriad :: Note -> Triad
makeMajorTriad root = (root, raise majorThird root, raise perfectFifth root)

makeMinorTriad :: Note -> Triad
makeMinorTriad root = (root, raise minorThird root, raise perfectFifth root)


findMood :: Triad -> Mood
findMood t@(root, third, fifth) =
    let thirdInterval = calcInterval root third
        fifthInterval = calcInterval root fifth
    in if thirdInterval == majorThird && fifthInterval == perfectFifth
       then Major
       else if thirdInterval == minorThird && fifthInterval == perfectFifth
            then Minor
            else error $ "Invalid triad: " ++ show root ++ " " ++ show third ++ " " ++ show fifth


--- The P transformation exchanges a triad for its Parallel. 
--- In a Major Triad move the third down a semitone (C major to C minor), 
--- in a Minor Triad move the third up a semitone (C minor to C major)
parallel :: Triad -> Triad
parallel (root, third, fifth) = let mood = findMood (root, third, fifth)
                                in case mood of
                                    Major -> (root, lower 1 third, fifth)
                                    Minor -> (root, raise 1 third, fifth)

-- The R transformation exchanges a triad for its Relative. 
-- In a Major Triad move the fifth up a tone (C major to A minor), 
-- in a Minor Triad move the root down a tone (A minor to C major)
relative :: Triad -> Triad
relative (root, third, fifth) = let mood = findMood (root, third, fifth)
                                    in case mood of
                                        Major ->  (raise 2 fifth, root , third)
                                        Minor ->  (third, fifth, lower 2 root)

-- The L transformation exchanges a triad for its Leading-Tone Exchange. 
-- In a Major Triad the root moves down by a semitone (C major to E minor), 
-- in a Minor Triad the fifth moves up by a semitone (E minor to C major)
leading :: Triad -> Triad
leading (root, third, fifth) = let mood = findMood (root, third, fifth)
                                        in case mood of
                                            Major ->   (third, fifth, lower 1 root)
                                            Minor ->   (raise 1 fifth, root, third)


slide :: Triad -> Triad
slide = leading >>> parallel >>> relative

nebenverwandt :: Triad -> Triad
nebenverwandt = relative >>> parallel >>> leading

hexapole :: Triad -> Triad
hexapole = leading >>> parallel >>> leading

-- Apply a single transform to a triad
applyTransform :: Transform -> Triad -> Triad
applyTransform Leading = leading
applyTransform Parallel = parallel
applyTransform Relative = relative
applyTransform Slide = slide
applyTransform Nebenverwandt = nebenverwandt
applyTransform Hexapole = hexapole

-- Apply a list of transforms to a triad, returning a list of triads
-- including the original triad and each transformed version
applyTransforms :: Triad -> [Transform] -> [Triad]
applyTransforms = scanl (flip applyTransform)

