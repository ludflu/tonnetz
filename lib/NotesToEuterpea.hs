module NotesToEuterpea where

import Euterpea 
import qualified NeoRiemann (Note(..), NoteClass(..), Triad(..)) 
import Diagrams (Renderable(render))

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
sound :: NeoRiemann.Note -> Dur -> Music Pitch
sound note' dur' = 
  note dur' (noteToEuterpea note')

-- | Play a NeoRiemann Triad as Euterpea Music Pitch
renderTriad :: Dur -> NeoRiemann.Triad -> Music Pitch
renderTriad dur' (NeoRiemann.Triad root third fifth _) = 
  -- Play the three notes as a chord (simultaneously)
  let chord' = chord  [sound root dur', 
         sound  third dur', 
         sound fifth dur' ]
    in chord'  :+: rest (1/4)


renderTriadSequence :: Dur -> [NeoRiemann.Triad] -> Music Pitch
renderTriadSequence dur' triads = 
  -- Create a sequence of triads
  let music = foldr1 (:+:) $ map (renderTriad dur') triads
  in music

playTriads :: [NeoRiemann.Triad] -> Integer -> IO ()
playTriads triads dur' = do
  let duration' = 1 / fromIntegral dur' :: Dur
      music = renderTriadSequence duration' triads 
   in play $ rest duration' :+: music :+: rest duration'

