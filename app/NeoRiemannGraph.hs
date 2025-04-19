{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
module NeoRiemannGraph where

import NeoRiemann
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Prelude


drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = (text noteTxt # fc black) # center <> circle 0.75 # fc white # center
              in node # scale 0.25


triangleVector :: (Floating f) => (V2 f, V2 f, V2 f)
triangleVector = let up = r2 (0,0.5)
                     downLeft = up # rotateBy (1/3)
                     downRight = up # rotateBy ((-1)/3)
                  in (up, downLeft, downRight)


convertVectorToPoint :: (Floating f) => V2 f -> P2 f
convertVectorToPoint v = let (x,y) = (v ^. _x, v ^. _y)
                          in p2 (x,y)

closeShape :: (Color a) => [V2 Double] ->  a -> Diagram B
closeShape pts c = let closedPts = map convertVectorToPoint pts
                    in strokeLoop (fromVertices closedPts) # fillColor c



drawMinorTriad :: Triad -> Diagram B
drawMinorTriad triad = let (root, third, fifth) = triad
                           (up,downLeft,downRight) = triangleVector
                           rootNode = drawNote root
                           thirdNode = drawNote third
                           fifthNode = drawNote fifth
                           triangle' = closeShape [up, downLeft, downRight, up] blue # center
                          --  triangle' = triangle 1.0
                           nodes = thirdNode # translate up
                            <> fifthNode # translate downRight
                            <> rootNode # translate downLeft
                        in (nodes # center <> triangle' ) # withEnvelope triangle' 

drawMajorTriad :: Triad -> Diagram B
drawMajorTriad triad = let (root, third, fifth) = triad
                           (up,downLeft,downRight) = triangleVector
                           (fup,fdownLeft,fdownRight) = (up # reflectY ,downLeft # reflectY ,downRight# reflectY )
                           rootNode = drawNote root
                           thirdNode = drawNote third
                           fifthNode = drawNote fifth
                           triangle' = closeShape [fup, fdownLeft, fdownRight, fup] red # center
                          --  triangle' = triangle 1.0 # reflectY
                           nodes = thirdNode # translate  fup
                            <> fifthNode # translate  fdownRight
                            <> rootNode #  translate  fdownLeft
                        in (nodes # center <> triangle' )  # withEnvelope  triangle'

drawTriad :: Triad -> Diagram B
drawTriad triad = let mood = findMood triad
                       in case mood of
                           Major -> drawMajorTriad triad
                           Minor -> drawMinorTriad triad

moveRight :: Triad -> Triad
moveRight t = case findMood t of
                  Major -> leading t
                  Minor -> NeoRiemann.relative t

moveLeft :: Triad -> Triad
moveLeft t = case findMood t of
               Major -> NeoRiemann.relative t
               Minor -> leading t

moveUp :: Triad -> Triad
moveUp t = case findMood t of
  Major -> parallel t
  Minor -> slide t

moveDown :: Triad -> Triad
moveDown t = case findMood t of
  Major -> slide t
  Minor -> parallel t

makeTriadColumn :: [Triad] -> Diagram B
makeTriadColumn ts = let triads = map drawTriad ts
                      in foldl1 (===) triads


--TODO - there's got to be a good way to clean up this CRAZY
drawTonnetez :: Triad -> Diagram B
drawTonnetez t = let mu2 = moveUp . moveUp
                     md2 = moveDown . moveDown
                     ml2 = moveLeft . moveLeft
                     mr2 = moveRight . moveRight
                     seed = [mu2 t, moveUp t, t, moveDown t, md2 t]                     
                     tonnetz :: [[Triad]] = [map ml2 seed, map moveLeft seed, seed, map moveRight seed, map mr2 seed]
                     tcols ::[Diagram B] =  map makeTriadColumn tonnetz
                     combineSnug l r = l # snugR <> r # snugL
                  in foldl1 combineSnug tcols
