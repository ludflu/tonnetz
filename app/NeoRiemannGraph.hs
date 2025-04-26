{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
module NeoRiemannGraph where

import NeoRiemann
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Backend.SVG (svgTitle)
import Diagrams.Prelude
import qualified Data.Map as M
import Data.Maybe
import Data.IntMap (mapAccum)
import Data.List
drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = (text noteTxt # fc black # scale 0.75) # center <> circle 0.75 # fc white # center
              in node # scale 0.25


getPoints :: (Floating n, Ord n) => Located (Trail V2 n) -> [V2 n]
getPoints t = let pts = map unp2 $ trailVertices t
               in reverse $ map (uncurry V2) pts               

triangleVector :: (Floating n, Ord n) => Located (Trail V2 n) -> (V2 n, V2 n, V2 n)
triangleVector t = let pts = getPoints t
                    in (head pts, pts !! 1 , pts !! 2)

convertVectorToPoint :: (Floating f) => V2 f -> P2 f
convertVectorToPoint v = let (x,y) = (v ^. _x, v ^. _y)
                          in p2 (x,y)

convertPointToVector :: (RealFloat f) =>  P2 f -> V2 f 
convertPointToVector p = let (x, y) = unp2 p
              in V2 x y

transformToVector :: (Floating f) => Transform -> V2 f
transformToVector t = case t of
  NeoRiemann.Identity ->  V2 0.0 0.0
  Leading -> unitX
  Parallel -> unitY
  Relative -> -unitX
  Slide ->  transformToVector Leading  + transformToVector Parallel + transformToVector Relative
  Nebenverwandt -> transformToVector Relative + transformToVector Parallel + transformToVector Leading  
  Hexapole -> transformToVector Leading  + transformToVector Parallel + transformToVector Leading  

findVector :: (RealFloat f) =>  Triad -> Transform -> V2 f
findVector triad trans = let mood = findMood triad
                             vec = transformToVector trans
                          in case mood of 
                            Major -> -vec
                            Minor -> vec

mapVectors :: (RealFloat f) => [(Triad, Transform)] -> [V2 f]
mapVectors  = map (uncurry findVector) 

closeShape :: [V2 Double] ->  Diagram B
closeShape pts = let closedPts = map convertVectorToPoint pts
                  in strokeLoop (fromVertices closedPts)


drawMinorTriad :: Triad -> Diagram B
drawMinorTriad triad = let Triad r t f _ = triad
                           t1 = triangle 1 
                           (up,downLeft,downRight) = triangleVector t1
                           rootNode = drawNote r
                           thirdNode = drawNote t
                           fifthNode = drawNote f
                           triangle' = closeShape [up, downLeft, downRight, up]  # center
                           nodes = thirdNode # translate up
                            <> fifthNode # translate downRight
                            <> rootNode # translate downLeft
                        in (nodes # center <> (t1  # fillColor blue # center # showOrigin)) # withEnvelope triangle'

drawMajorTriad :: Triad -> Diagram B
drawMajorTriad triad = let Triad r t f _ = triad
                           t1 = triangle 1 # reflectY
                           (up,downLeft,downRight) = triangleVector t1
                           rootNode = drawNote r
                           thirdNode = drawNote t
                           fifthNode = drawNote f
                           triangle' = closeShape [up, downLeft, downRight, up]  # center
                           nodes = thirdNode # translate up
                            <> fifthNode # translate  downRight
                            <> rootNode #  translate  downLeft
                        in (nodes # center <> t1 # fillColor red # center # showOrigin)  # withEnvelope triangle'

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
                            withName = if null crumbs then "origin" else ""
                            diag = case mood of
                                     Major -> drawMajorTriad triad
                                     Minor -> drawMinorTriad triad
                         in labeled (diag # svgTitle breadcrumbStr # named withName ) nbr

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


--makeVects :: [Int] -> [Int]
makeVects vs = let indx = [0..length vs]
                   takes = flip take vs
                   groups = map takes indx
                in map sum groups

windows' :: Int -> [a] -> [[a]]
windows' n = map (take n) . tails


makeTheArrows :: [(Triad, Transform)] -> Diagram B
makeTheArrows tt = let vecs = mapVectors tt
                       vecsums = makeVects vecs
                       pairs = windows' 2 vecsums
                       points = map (\ls ->  p2 (head ls , ls !! 1)) pairs
                       pointPairs' = windows' 2 points
                       pointPairs'' = map (\ls ->  (head ls , convertPointToVector (ls !! 1))) pointPairs'
                       arrowz =  map (uncurry arrowAt) pointPairs''
                    in foldl1 (<>) arrowz 


--draws a tonnez with the passed in triad as the center / origin
drawTonnetez :: Triad -> [(Triad, Transform)] -> Int -> M.Map String Int -> Diagram B
drawTonnetez t tt contextSize labels = let ups :: [Triad -> Triad] = map (composeN  moveUp) (reverse [1..contextSize])
                                           downs :: [Triad -> Triad] = map (composeN  moveDown)  [1..contextSize]                                                          
                                           seed :: [Triad] = map ($ t) (ups ++ [id] ++ downs) -- this is the middle column
                                           lefts :: [Triad -> Triad] = map (composeN  moveLeft) (reverse [1..contextSize])
                                           rights :: [Triad -> Triad] = map (composeN  moveRight) [1..contextSize]
                                           columnTransforms :: [Triad -> Triad] = lefts ++ [id] ++ rights
                                           tonnetz = ctf seed columnTransforms
                                           tcols ::[Diagram B] =  map (makeTriadColumn labels) tonnetz
                                           combineSnug l r = l # snugR <> r # snugL  --ensure triangle fit together by draw the diagrams snug against each other,  following the shape's envelope/trace
                                           tonnetzDiagram = foldl1 combineSnug tcols
                                           arrowDiagram = makeTheArrows tt
                                          --  originTriadDiagram = lookupName "origin" tonnetzDiagram
                                          --  originLocation = fmap location originTriadDiagram
                                          --  triadOrigin = fromMaybe (p2 (0,0)) originLocation

                                       in  tonnetzDiagram <> arrowDiagram
