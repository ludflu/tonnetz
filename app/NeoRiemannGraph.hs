{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
module NeoRiemannGraph where

import NeoRiemann
    ( findMood,
      leading,
      parallel,
      relative,
      slide,
      Mood(Minor, Major),
      Note(noteClass),
      Transform(..),
      Triad(Triad, breadcrumbs) )
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Backend.SVG (svgTitle)
import Diagrams.Prelude
import qualified Data.Map as M

import Data.Maybe

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

drawNote :: Note -> Diagram B
drawNote n = let noteTxt = show $ noteClass n
                 node = (text noteTxt # fc black # scale 0.75) # center <> circle 0.75 # lwL 0.01 # fc white # center
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
  Leading ->  unitY # rotateBy (3/8)  -- -(unitX # scaleX (3/4))
  Parallel -> unitY  
  Relative -> unitY # rotateBy (5/8)
  Slide ->  transformToVector Leading  + transformToVector Parallel + transformToVector Relative
  Nebenverwandt -> transformToVector Relative + transformToVector Parallel + transformToVector Leading  
  Hexapole -> transformToVector Leading  + transformToVector Parallel + transformToVector Leading  

findVector :: (RealFloat f) =>  Triad -> Transform -> V2 f
findVector triad trans = let mood = findMood triad
                             vec = transformToVector trans
                          in case mood of 
                            Major -> vec
                            Minor -> -vec

mapVectors :: (RealFloat f) => [(Triad, Transform)] -> [V2 f]
mapVectors  = map (uncurry findVector) 

closeShape :: [V2 Double] ->  Diagram B
closeShape pts = let closedPts = map convertVectorToPoint pts
                  in strokeLoop (fromVertices closedPts)

drawTriad :: Triad -> Diagram B
drawTriad triad = let Triad r t f _ = triad
                      mood = findMood triad
                      tcolor = case mood of 
                             Minor ->  blue
                             Major ->  red
                      t1 = case mood of 
                             Minor -> triangle 1 
                             Major -> triangle 1 # reflectY
                      (left,up,right) = triangleVector t1
                      root  = drawNote r # translate left
                      third = drawNote t # translate up
                      fifth = drawNote f # translate right
                      withTriangleEnvelope = withEnvelope $ closeShape (getPoints t1)  # center
                      nodes = root <> third <> fifth 
                   in (nodes # center <> t1 # fillColor tcolor # center # showOrigin)  # withTriangleEnvelope


labeled :: Diagram B -> Maybe Int -> Diagram B
labeled d Nothing = d
labeled d (Just s) =  text (show s) # fontSize (local 0.25) # fc green # center <> d --black # translate (r2 (0, -0.5)) # center

makeName :: Triad -> String
makeName t = let chordName = show t
                 crumbs = breadcrumbs t
                 trail = show (reverse (breadcrumbs t))
                 fullName = chordName ++ ": " ++ trail
                 name = if null crumbs then "origin" else fullName
              in name


drawLabeledTriad ::  M.Map String Int -> Triad -> Diagram B
drawLabeledTriad label triad = let nbr = M.lookup (show triad) label
                                   name = makeName triad
                                   diag = drawTriad triad
                                in labeled (diag # svgTitle name # named name ) nbr

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
makeTriadColumn labels ts = let triads = map (drawLabeledTriad labels) ts
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


makeVects :: Floating b => [V2 b] -> [V2 b]
makeVects vs = let indx = [0..length vs]
                   takes = flip take vs
                   groups = map takes indx
                in map sum groups

windows :: Int -> [a] -> [[a]]
windows n0 = go 0 Seq.empty
  where
    go n s (a:as) | n' <  n0   =              go n' s'  as
                  | n' == n0   = toList s'  : go n' s'  as
                  | otherwise =  toList s'' : go n  s'' as
      where
        n'  = n + 1         -- O(1)
        s'  = s Data.Sequence.|> a        -- O(1)
        s'' = Seq.drop 1 s' -- O(1)
    go _ _ [] = []

makeTheArrows :: [V2 Double] -> Diagram B
makeTheArrows vecs = let vecsums =  V2 0.0 0.0 : vecs
                         pairs = windows 2 vecsums
                         linez :: [(P2 Double,V2 Double )] = map (\ls ->  (convertVectorToPoint (head ls) , ls !! 1)) pairs
                         arrowz  =  map (uncurry arrowAt) linez
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
                                           
                                          --  vecs  = mapVectors tt
                                          --  originTriad = lookupName "origin" tonnetzDiagram
                                          --  originLocation = fmap location originTriad
                                          --  triadOrigin = fromMaybe (p2 (0,0)) originLocation
                                          --  arrowDiagram = makeTheArrows vecs                                          
                                      --  in  arrowDiagram # translate (convertPointToVector triadOrigin) <> tonnetzDiagram
                                       in  tonnetzDiagram
