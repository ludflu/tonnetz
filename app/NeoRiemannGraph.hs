{-# LANGUAGE NoMonomorphismRestriction #-}

module NeoRiemannGraph where

import NeoRiemann
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Prelude 


drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = circle 0.75 # center <> (text noteTxt # fc black) # center
              in node # scale 0.25


triangleVector :: (Floating f) => (V2 f, V2 f, V2 f)
triangleVector = let up = r2 (0,0.5)
                     downLeft = up # rotateBy (1/3)
                     downRight = up # rotateBy ((-1)/3)
                  in (up, downLeft, downRight)


drawMinorTriad :: Triad -> Diagram B
drawMinorTriad triad = let (root, third, fifth) = triad
                           (up,downLeft,downRight) = triangleVector
                           rootNode = drawNote root
                           thirdNode = drawNote third
                           fifthNode = drawNote fifth
                        in thirdNode # translate up
                            <> fifthNode # translate downRight
                            <> rootNode # translate downLeft

drawMajorTriad :: Triad -> Diagram B
drawMajorTriad triad = let (root, third, fifth) = triad
                           (up,downLeft,downRight) = triangleVector
                           rootNode = drawNote root
                           thirdNode = drawNote third
                           fifthNode = drawNote fifth
                        in thirdNode # translate  (up # reflectY)
                            <> fifthNode # translate  (downRight # reflectY)
                            <> rootNode #  translate  (downLeft # reflectY)

drawTriad :: Triad -> Diagram B
drawTriad triad = let mood = findMood triad
                       in case mood of
                           Major -> drawMajorTriad triad
                           Minor -> drawMinorTriad triad