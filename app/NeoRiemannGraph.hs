{-# LANGUAGE NoMonomorphismRestriction #-}

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
                           nodes = thirdNode # translate  fup
                            <> fifthNode # translate  fdownRight
                            <> rootNode #  translate  fdownLeft
                        in (nodes # center <> triangle' )  # withEnvelope triangle' 

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


drawTonnetez :: Triad -> Diagram B
drawTonnetez t = (drawTriad ((moveLeft . moveUp) t) === drawTriad (moveLeft t) === drawTriad ((moveLeft . moveDown) t)) #snugR
                  ||| (drawTriad (moveUp t) === drawTriad t === drawTriad (moveDown t)) #snugL
                  -- ||| (drawTriad ((moveRight . moveUp) t) === drawTriad (moveRight t) === drawTriad ((moveRight . moveDown) t))