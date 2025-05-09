module NotesToEuterpea where

import Euterpea 
import qualified NeoRiemann (Note(..), NoteClass(..), Triad(..), raise) 

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
sound :: Dur -> NeoRiemann.Note -> Music Pitch
sound dur' note' = 
  note dur' (noteToEuterpea note')

-- | Play a NeoRiemann Triad as Euterpea Music Pitch
renderTriad :: Dur -> NeoRiemann.Triad -> Music Pitch
renderTriad dur' (NeoRiemann.Triad root third fifth _) = 
  -- Play the three notes as a chord (simultaneously)
  let chord' = chord  [sound dur' root , 
         sound  dur' third , 
         sound dur' fifth  ]
    in chord'  :+: rest (1/4)

renderArpTriad :: Dur -> NeoRiemann.Triad -> Music Pitch
renderArpTriad dr (NeoRiemann.Triad root third fifth _) = 
  -- Play the three notes as a chord (simultaneously)
  let chord' = chord  [sound dr root , 
         sound  dr third , 
         sound dr fifth  ]
      upOctave = flip NeoRiemann.raise  12
      mel = map (sound (dr/4) . upOctave ) [root , third , fifth]
      melMuic = foldl1 (:+:) mel

    in (melMuic :=: chord'):+:  rest (1/4)
    
renderTriadSequence :: Dur -> [NeoRiemann.Triad] -> Music Pitch
renderTriadSequence dur' triads =  foldr1 (:+:) $ map (renderTriad dur') triads

renderArpTriadSequence :: Dur -> [NeoRiemann.Triad] -> Music Pitch
renderArpTriadSequence dr triads =  foldr1 (:+:) $ map (renderArpTriad dr) triads


playTriads :: [NeoRiemann.Triad] -> Integer -> IO ()
playTriads triads dur' = do
  let duration' = 1 / fromIntegral dur' :: Dur
      music = renderArpTriadSequence duration' triads 
   in play $ rest duration' :+: music :+: rest duration'


writeTriads :: FilePath -> [NeoRiemann.Triad] -> Integer -> IO ()
writeTriads fp triads dur' = do
  let duration' = 1 / fromIntegral dur' :: Dur
      music = renderArpTriadSequence duration' triads 
   in writeMidi fp $ rest duration' :+: music :+: rest duration'
