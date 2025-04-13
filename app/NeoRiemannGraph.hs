{-# LANGUAGE NoMonomorphismRestriction #-}

module NeoRiemannGraph where

import NeoRiemann
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Prelude 


drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = (text noteTxt # fc black) # center <> circle 0.75 # fillColor white # center
              in node # scale 0.25


triangleVector :: (Floating f) => (V2 f, V2 f, V2 f)
triangleVector = let up = r2 (0,0.5)
                     downLeft = up # rotateBy (1/3)
                     downRight = up # rotateBy ((-1)/3)
                  in (up, downLeft, downRight)


convertVectorToPoint :: (Floating f) => V2 f -> P2 f
convertVectorToPoint v = let (x,y) = (v ^. _x, v ^. _y)
                          in p2 (x,y)

closeShape :: [V2 Double] -> Diagram B 
closeShape pts = let closedPts = map convertVectorToPoint pts                
                  in strokeLoop (fromVertices closedPts) # fillColor red  


                   
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
                           (fup,fdownLeft,fdownRight) = (up # reflectY ,downLeft # reflectY ,downRight# reflectY )
                           rootNode = drawNote root
                           thirdNode = drawNote third
                           fifthNode = drawNote fifth
                           triangle' = closeShape [fup, fdownLeft, fdownRight, fup]
                           nodes = thirdNode # translate  fup
                            <> fifthNode # translate  fdownRight 
                            <> rootNode #  translate  fdownLeft 
                        in nodes #center <> triangle' # center 

drawTriad :: Triad -> Diagram B
drawTriad triad = let mood = findMood triad
                       in case mood of
                           Major -> drawMajorTriad triad
                           Minor -> drawMinorTriad triad