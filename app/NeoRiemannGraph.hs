{-# LANGUAGE NoMonomorphismRestriction #-}

module NeoRiemannGraph where

import NeoRiemann (Note, Triad)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude (Diagram, center, circle, p2, position, r2, text, translate, (#))
import Text.Show.Functions


bla :: Note -> String
bla = show

drawTonnetz :: Triad -> Diagram B
drawTonnetz triad =
  let (root, third, fifth) = triad
   in circle 0.1 <> center (text  $ show root) 
  --  # translate (r2 (0, 0)) 
      -- <> center (text $ show third) # translate (r2 (1, 0))
      -- <> center (text $ show fifth) # translate (r2 (0, 1))
   