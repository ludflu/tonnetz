{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NeoRiemann where

import Control.Arrow ( (>>>) )


type Interval = Int

data Transform = Identity | Leading | Parallel | Relative | Slide | Nebenverwandt | Hexapole
  deriving (Show, Eq, Ord)

data NoteClass = C | Cs| D| Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Ord, Enum)

newtype ScaleFamily = ScaleFamily [Int]

type Es = F
type Bs = C

data Note = Note { noteClass :: NoteClass, octave :: Int }
    deriving (Eq, Ord)

instance Show Note where
    show (Note nc oct) = show nc ++ show oct

data Mood = Major | Minor
    deriving (Show, Eq)

data Triad = Triad { root :: Note, third :: Note, fifth :: Note, breadcrumbs :: [Transform] }
    deriving (Eq, Ord)

instance Show Triad where
    show (Triad r t f crumbs) = "Triad: " ++ show r ++ " " ++ show t ++ " " ++ show f 

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

halfStep :: Interval
halfStep = 1

wholeStep :: Interval
wholeStep = 2

minorThird :: Interval
minorThird = 3

majorThird :: Interval
majorThird = 4

perfectFifth :: Interval
perfectFifth = 7

calcSemitones :: Note -> Interval
calcSemitones n =  let o = octave n
                       nc = noteClass n
                    in fromEnum nc + 12 * o


raise :: Note -> Interval -> Note
raise n i  = let
                translatedNoteIdx = fromEnum (noteClass n) + i
                translatedNote = toEnum (translatedNoteIdx `mod` 12)
                translatedOctave = octave n + translatedNoteIdx `div` 12
             in Note translatedNote translatedOctave

lower :: Note -> Interval -> Note
lower n i = raise n (-i)

calcInterval :: Note -> Note -> Interval
calcInterval n1 n2 = let i = calcSemitones n2 - calcSemitones n1
                      in i `mod` 12

makeMajorTriad :: Note -> Triad
makeMajorTriad r = Triad r (raise r majorThird ) (raise r perfectFifth) []

makeMinorTriad :: Note -> Triad
makeMinorTriad r = Triad r (raise r minorThird) (raise r perfectFifth) []


findMood :: Triad -> Mood
findMood (Triad r t f _) =
    let thirdInterval = calcInterval r t
        fifthInterval = calcInterval r f
    in if thirdInterval == majorThird && fifthInterval == perfectFifth
       then Major
       else if thirdInterval == minorThird && fifthInterval == perfectFifth
            then Minor
            else error $ "Invalid triad: " ++ show r ++ " " ++ show t ++ " " ++ show f

removeDupes :: [Transform] -> [Transform]
removeDupes [] = []
removeDupes [x] = [x]
removeDupes (x:y:xs)
  | x == y    = removeDupes xs  -- Remove both instances of consecutive duplicates
  | otherwise = x : removeDupes (y:xs)

cleanCrumbs :: Triad -> Triad
cleanCrumbs (Triad r t f crumbs) = Triad r t f (removeDupes crumbs)


--- The P transformation exchanges a triad for its Parallel. 
--- In a Major Triad move the third down a semitone (C major to C minor), 
--- in a Minor Triad move the third up a semitone (C minor to C major)
parallel :: Triad -> Triad
parallel triad@(Triad r t f crumbs) = let mood = findMood triad
                                       in case mood of
                                           Major -> Triad r (lower t 1 ) f $ removeDupes (Parallel : crumbs)
                                           Minor -> Triad r (raise t 1) f $ removeDupes (Parallel : crumbs)



-- The R transformation exchanges a triad for its Relative. 
-- In a Major Triad move the fifth up a tone (C major to A minor), 
-- in a Minor Triad move the root down a tone (A minor to C major)
relative :: Triad -> Triad
relative triad@(Triad r t f crumbs) = let mood = findMood triad
                                       in case mood of
                                           Major -> Triad (raise f 2) r t $ removeDupes (Relative : crumbs)
                                           Minor -> Triad t f (lower r 2) $ removeDupes (Relative : crumbs)



-- The L transformation exchanges a triad for its Leading-Tone Exchange. 
-- In a Major Triad the root moves down by a semitone (C major to E minor), 
-- in a Minor Triad the fifth moves up by a semitone (E minor to C major)
leading :: Triad -> Triad
leading triad@(Triad r t f crumbs) = let mood = findMood triad
                                     in case mood of
                                         Major -> Triad t f (lower r 1) $ removeDupes (Leading : crumbs)
                                         Minor -> Triad (raise f 1)  r t $ removeDupes (Leading : crumbs)



slide :: Triad -> Triad
slide triad = let tfm = leading >>> parallel >>> relative
                  moved = tfm triad
               in moved { breadcrumbs = removeDupes (breadcrumbs moved) }


nebenverwandt :: Triad -> Triad
nebenverwandt triad = let tfm = relative >>> leading >>> parallel
                          moved = tfm triad
                       in moved { breadcrumbs = removeDupes (breadcrumbs moved) }

hexapole :: Triad -> Triad
hexapole triad = let tfm = leading >>> parallel >>> leading
                     moved = tfm triad
                  in moved { breadcrumbs = removeDupes (breadcrumbs moved) }

-- Apply a single transform to a triad
applyTransform :: Transform -> Triad -> Triad
applyTransform Identity = id
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

allTransformations :: [Transform]
allTransformations = [Leading, Parallel, Relative, Slide, Nebenverwandt, Hexapole]

major :: ScaleFamily
major= ScaleFamily [wholeStep, wholeStep, halfStep, wholeStep, wholeStep, wholeStep, halfStep, wholeStep]
minor :: ScaleFamily
minor= ScaleFamily [wholeStep, halfStep, wholeStep, wholeStep, halfStep, wholeStep, wholeStep, wholeStep]

majorPentatonic :: ScaleFamily
majorPentatonic= ScaleFamily [wholeStep, wholeStep, minorThird, wholeStep, majorThird]
minorPentatonic :: ScaleFamily
minorPentatonic= ScaleFamily [minorThird, wholeStep, wholeStep, minorThird, wholeStep]

patternToSemitones :: [Int] -> [Int]
patternToSemitones pat = init $ scanl1 (+) (0:pat)

-- given a scale pattern, a root note, and a duration, return the scale of notes
makeScale :: ScaleFamily -> Note ->  [Note]
makeScale (ScaleFamily ptn) n  = let translateNote = raise n 
                                     semis = patternToSemitones ptn
                                  in map translateNote semis
