{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
module NeoRiemannGraph where

import NeoRiemann
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Backend.SVG (renderSVG, svgTitle)
import Diagrams.Prelude
import qualified Data.Map as M

drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = (text noteTxt # fc black # scale 0.75) # center <> circle 0.75 # fc white # center
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
drawMinorTriad triad = let Triad r t f _ = triad
                           (up,downLeft,downRight) = triangleVector
                           rootNode = drawNote r
                           thirdNode = drawNote t
                           fifthNode = drawNote f
                           triangle' = closeShape [up, downLeft, downRight, up] blue # center
                          --  triangle' = triangle 1.0
                           nodes = thirdNode # translate up
                            <> fifthNode # translate downRight
                            <> rootNode # translate downLeft
                        in (nodes # center <> triangle' ) # withEnvelope triangle'

drawMajorTriad :: Triad -> Diagram B
drawMajorTriad triad = let Triad r t f _ = triad
                           (up,downLeft,downRight) = triangleVector
                           (fup,fdownLeft,fdownRight) = (up # reflectY ,downLeft # reflectY ,downRight# reflectY )
                           rootNode = drawNote r
                           thirdNode = drawNote t
                           fifthNode = drawNote f
                           triangle' = closeShape [fup, fdownLeft, fdownRight, fup] red # center
                          --  triangle' = triangle 1.0 # reflectY
                           nodes = thirdNode # translate  fup
                            <> fifthNode # translate  fdownRight
                            <> rootNode #  translate  fdownLeft
                        in (nodes # center <> triangle' )  # withEnvelope  triangle'

labeled :: Diagram B -> Maybe Int -> Diagram B
labeled d Nothing = d
labeled d (Just s) = d # opacity 0.5 <> text (show s) # fontSize (local 0.25) # fc green # center --black # translate (r2 (0, -0.5)) # center

drawTriad ::  M.Map String Int -> Triad -> Diagram B
drawTriad label triad = let mood = findMood triad
                            nbr = M.lookup (show triad) label
                            Triad _ _ _ crumbs = triad
                            breadcrumbStr = if null crumbs
                                           then "No transformations"
                                           else "Transformations: " ++ show (reverse crumbs)
                            diag = case mood of
                                     Major -> drawMajorTriad triad
                                     Minor -> drawMinorTriad triad
                         in labeled (diag # svgTitle breadcrumbStr) nbr

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

makeTriadColumn :: M.Map String Int -> [Triad] ->  Diagram B
makeTriadColumn labels ts = let triads = map (drawTriad labels) ts
                             in foldl1 (===) triads

ctf :: [Triad] -> [Triad -> Triad] -> [[Triad]]
ctf ts = map (`map` ts)

-- | Compose a function with itself n times
-- For n = 0, returns the identity function
-- For n > 0, returns f composed with itself n times
composeN :: (a -> a) -> Int -> (a -> a)
composeN  _ 0 = id
composeN  f 1 = f
composeN  f n = f . composeN f (n-1) 

drawTonnetez :: Triad -> Int -> M.Map String Int -> Diagram B
drawTonnetez t contextSize labels = let ups :: [Triad -> Triad] = map (composeN  moveUp) (reverse [1..contextSize])
                                        downs :: [Triad -> Triad] = map (composeN  moveDown)  [1..contextSize]                  
                                        -- this is the middle column
                                        seed :: [Triad] = map ($ t) (ups ++ [id] ++ downs)
                                        lefts :: [Triad -> Triad] = map (composeN  moveLeft) (reverse [1..contextSize])
                                        rights :: [Triad -> Triad] = map (composeN  moveRight) [1..contextSize]
                                        columnTransforms :: [Triad -> Triad] = lefts ++ [id] ++ rights
                                        tonnetz = ctf seed columnTransforms
                                        tcols ::[Diagram B] =  map (makeTriadColumn labels) tonnetz
                                        combineSnug l r = l # snugR <> r # snugL
                                      in foldl1 combineSnug tcols
