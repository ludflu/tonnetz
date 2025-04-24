module NotesToEuterpea where

import qualified Euterpea as E
import NeoRiemann (Note(..), NoteClass(..), Triad(..), Transform(..), applyTransforms)

-- | Convert a NeoRiemann Note to Euterpea's Pitch
noteToEuterpea :: NeoRiemann.Note -> E.Pitch
noteToEuterpea (Note nc oct) = 
  let pc = case nc of
        C  -> E.C
        Cs -> E.Cs
        D  -> E.D
        Ds -> E.Ds
        E  -> E.E
        F  -> E.F
        Fs -> E.Fs
        G  -> E.G
        Gs -> E.Gs
        A  -> E.A
        As -> E.As
        B  -> E.B
  in (pc,oct)

-- | Convert a NeoRiemann Note to Euterpea's Music Pitch
neoRiemannToEuterpea :: NeoRiemann.Note -> E.Music E.Pitch
neoRiemannToEuterpea note = 
  -- Create a quarter note with the pitch derived from the NeoRiemann Note
  E.note (1/4) (noteToEuterpea note)

-- | Play a NeoRiemann Triad as Euterpea Music Pitch
playTriad :: NeoRiemann.Triad -> E.Music E.Pitch
playTriad (Triad root third fifth _) = 
  -- Play the three notes as a chord (simultaneously)
  E.chord [neoRiemannToEuterpea root, 
         neoRiemannToEuterpea third, 
         neoRiemannToEuterpea fifth]

-- -- | Play a sequence of NeoRiemann Triads as Euterpea Music Pitch
-- playTriadSequence :: [NeoRiemann.Triad] -> Euterpea.Music Pitch
-- playTriadSequence = foldr1 (:+:) . map playTriad

-- | Example of creating a simple Music Pitch value
-- Example: c 4 qn = quarter note middle C
-- C major chord example:
-- cMajorChord :: Music Pitch
-- cMajorChord = chord [c 4 qn, e 4 qn, g 4 qn]
--   where
--     -- Helper functions for creating note values
--     c o d = note d (Pitch C o)    -- C note of octave o, duration d
--     e o d = note d (Pitch E o)    -- E note of octave o, duration d
--     g o d = note d (Pitch G o)    -- G note of octave o, duration d
--     qn = 1/4                      -- Quarter note duration
