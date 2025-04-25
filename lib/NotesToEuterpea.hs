module NotesToEuterpea where

import Euterpea 
import qualified NeoRiemann (Note(..), NoteClass(..), Triad(..)) 

-- | Convert a NeoRiemann Note to Euterpea's Pitch
noteToEuterpea :: NeoRiemann.Note -> Pitch
noteToEuterpea ( NeoRiemann.Note nc oct) = 
  let pc = case nc of
        NeoRiemann.C  -> C
        NeoRiemann.Cs -> Cs
        NeoRiemann.D  -> D
        NeoRiemann.Ds -> Ds
        NeoRiemann.E  -> E
        NeoRiemann.F  -> F
        NeoRiemann.Fs -> Fs
        NeoRiemann.G  -> G
        NeoRiemann.Gs -> Gs
        NeoRiemann.A  -> A
        NeoRiemann.As -> As
        NeoRiemann.B  -> B
  in (pc,oct)

-- | Convert a NeoRiemann Note to Euterpea's Music Pitch
neoRiemannToEuterpea :: NeoRiemann.Note -> Music Pitch
neoRiemannToEuterpea note' = 
  -- Create a quarter note with the pitch derived from the NeoRiemann Note
  note (1/4) (noteToEuterpea note')

-- | Play a NeoRiemann Triad as Euterpea Music Pitch
renderTriad :: NeoRiemann.Triad -> Music Pitch
renderTriad (NeoRiemann.Triad root third fifth _) = 
  -- Play the three notes as a chord (simultaneously)
  chord [neoRiemannToEuterpea root, 
         neoRiemannToEuterpea third, 
         neoRiemannToEuterpea fifth]

-- -- | Play a sequence of NeoRiemann Triads as Euterpea Music Pitch
renderTriadSequence :: [NeoRiemann.Triad] -> Music Pitch
renderTriadSequence = foldr1 (:+:) . map renderTriad


playTriads :: [NeoRiemann.Triad] -> IO ()
playTriads triads = do
  let music = renderTriadSequence triads
   in play $ rest (1/4) :+: music :+: rest (1/4)

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
