{-# LANGUAGE NoMonomorphismRestriction #-}

module NeoRiemannGraph where

import NeoRiemann (Note, Triad, noteClass)
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Prelude 


drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = circle 0.75 # center <> (text noteTxt # fc black) # center
              in node # scale 0.25

drawMinorTriad :: Triad -> Diagram B
drawMinorTriad triad = let (root, third, fifth) = triad
                           up = r2 (0,0.5)
                           downLeft = up # rotateBy (1/3)
                           downRight = up # rotateBy ((-1)/3)
                           rootNode = drawNote root
                           thirdNode = drawNote third
                           fifthNode = drawNote fifth
                        in thirdNode # translate up
                            <> fifthNode # translate downRight
                            <> rootNode # translate downLeft
                            <> circle 0.1 # fc black # translate (r2 (0, 0))
                         --  # translate (r2 (0, 0)) 
      -- <> center (text $ show third) # translate (r2 (1, 0))
      -- <> center (text $ show fifth) # translate (r2 (0, 1))
