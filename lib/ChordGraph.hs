{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module ChordGraph (transforms, noteMap, Tone, findChordProgression, printFlat) where

import Control.Arrow
import Control.Monad (forM)
import Data.Array.IO
import Data.Graph.Inductive (Graph (match))
import qualified Data.Graph.Inductive as G
import qualified Data.List as DL
import Data.List.Unique (sortUniq)
import qualified Data.Map as Map
import Data.Maybe
import Data.Modular (ℤ, type (/))
import System.Random
  ( RandomGen,
    randomR,
    randomRIO,
  )

data TriadicTransform = Leading | Relative | Parallel | Garbage deriving (Show, Eq, Ord)

-- | Slide | Nebenverwandt | Hexpole
type TriadNodeLabel = [String] -- the node is the set of notes in the triad

type TriadEdgeLabel = TriadicTransform -- the edge is the difference in magnitude of the tone being swapped
-- out compared to the tone being swapped in

type TriadGraph = G.Gr TriadNodeLabel TriadEdgeLabel

type Transform = (TriadNodeLabel -> TriadNodeLabel)

type Tone = ℤ / 12


notes :: [String]
notes = [c, cs, d, ds, e, f, fs, g, gs, a, as, b]

c = "C"

cs = "C#"

d = "D"

db = cs

ds = "D#"

eb = ds

e = "E"

fb = e

f = "F"

es = f

fs = "F#"

gb = fs

g = "G"

gs = "G#"

ab = gs

a = "A"

as = "A#"

bb = as

b = "B"

bs = c

cb = b

notetriads :: [[String]]
notetriads =
  sortUniq
    [ [c, e, g], -- CMAJOR
      [cs, es, gs], -- C# MAJOR
      [db, f, ab], -- Db Major
      [d, fs, a], -- D MAJOR
      [ds, g, as], -- D# MAJOR
      [eb, g, bb], -- Eb Major
      [e, gs, b], -- E Major
      [f, a, c], -- F Major
      [fs, as, cs], -- F# Major
      [gb, bb, db], -- Gb Major
      [g, b, d], -- G Major
      [gs, bs, ds], -- G# Major
      [ab, c, eb], -- Ab Major
      [a, cs, e], -- A Major
      [as, d, es], -- A# Major
      [bb, d, f], -- Bb Major
      [b, ds, fs], -- B Major
      [c, eb, g], -- C Minor
      [cs, e, gs], -- C# minor
      [db, fb, ab], -- Db Minor
      [d, f, a], -- D Minor
      [ds, fs, as], -- D# Minor
      [eb, gb, bb], -- Eb Minor
      [e, g, b], -- E Minor
      [f, ab, c], -- F Minor
      [fs, a, cs], -- F# Minor
      [gb, a, db], -- Gb Minor
      [g, bb, d], -- G Minor
      [gs, b, ds], -- G# Minor
      [ab, cb, eb], -- Ab Minor
      [a, c, e], -- A Minor
      [as, cs, es], -- A# Minor
      [bb, db, f], -- Bb Minor
      [b, d, fs] -- B Minor
    ]

type Triad = (Tone, Tone, Tone)

nodeLookup :: Map.Map TriadNodeLabel Int
nodeLookup =
  let triadsWithIndex = zip notetriads [0 ..]
   in Map.fromList triadsWithIndex

nodeLookup' :: Map.Map Int TriadNodeLabel
nodeLookup' =
  let triadsWithIndex = zip [0 ..] notetriads
   in Map.fromList triadsWithIndex

noteMap :: Map.Map Tone String
noteMap = Map.fromList $ zip [0 .. 11] notes

noteMap' :: Map.Map String Tone
noteMap' = Map.fromList $ zip notes [0 .. 11]

mapTriad' :: [String] -> Triad
mapTriad' notes =
  let (a, b, c) = (head notes, notes !! 1, notes !! 2)
   in (noteMap' Map.! a, noteMap' Map.! b, noteMap' Map.! c)

-- debugHead :: [a] -> a
-- debugHead l = l !! 0

hasEdge :: TriadNodeLabel -> TriadNodeLabel -> Bool
hasEdge triad1 triad2 =
  let sharedNotes = DL.intersect triad1 triad2
   in length sharedNotes == 2

findMates :: [TriadNodeLabel] -> TriadNodeLabel -> [TriadNodeLabel]
findMates triads triad = DL.filter (hasEdge triad) triads

neighborChords :: [[TriadNodeLabel]]
neighborChords =
  let cfinder = findMates notetriads
   in map cfinder notetriads

makePairs :: a -> [a] -> [(a, a)]
makePairs item = map (, item)

triadEdges :: [([String], [String])]
triadEdges =
  let es = map (\t -> (t, findMates notetriads t)) notetriads
      pairs = concatMap (uncurry makePairs) es
   in pairs

makeTriadEdge :: TriadNodeLabel -> TriadNodeLabel -> (Int, Int, TriadicTransform)
makeTriadEdge from to =
  let fromIdx = nodeLookup Map.! from
      toIdx = nodeLookup Map.! to
      edgelabel = fromMaybe Garbage $ findChanged' from to
   in (fromIdx, toIdx, edgelabel)

triadGraph :: TriadGraph
triadGraph = G.mkGraph nodes edges
  where
    nodes = zip [0 ..] notetriads
    edges = map (uncurry makeTriadEdge) triadEdges

triadFinder :: Int -> TriadNodeLabel
triadFinder idx = nodeLookup' Map.! idx

findNeighbors :: TriadNodeLabel -> [TriadNodeLabel]
findNeighbors triad =
  let tidx = nodeLookup Map.! triad
      ns = sortUniq $ G.neighbors triadGraph tidx
   in map triadFinder ns

toneInterval :: Tone -> Tone -> Tone
toneInterval a b = a - b

untuple :: Triad -> [Tone]
untuple (a, b, c) = [a, b, c]

findChanged' :: [String] -> [String] -> Maybe TriadicTransform
findChanged' t1 t2 =
  let t1' = mapTriad' t1
      t2' = mapTriad' t2
      changeInterval = findChanged t1' t2'
   in describeTransform t1' t2'

-- this function takes two triads and finds the changed note
-- it returns the interval between the changed note and the original note
findChanged :: Triad -> Triad -> Maybe Tone
findChanged t1 t2 =
  let as :: [Tone] = untuple t1
      bs :: [Tone] = untuple t2
      common :: [Tone] = DL.intersect as bs
      tnote1 = filter (`DL.notElem` common) as
      tnote2 = filter (`DL.notElem` common) bs
   in do
        n1 <- listToMaybe tnote1
        n2 <- listToMaybe tnote2
        return $ toneInterval n2 n1

-- this function will take the interval (expressed as a module 12 integer) and
-- the boolean value of whether the root note is different
-- and return the type of transformation
-- if the interval is 2 or 10, it is a relative transformation
-- if the interval is 1 or 11, it is a leading transformation
-- if the interval is 0 or 12, it is a parallel transformation (unison or octave)
-- otherwise Nothing
makeTxform :: Tone -> Bool -> Maybe TriadicTransform
makeTxform delta rootDiffers
  | delta == 2 || delta == 10 = Just Relative
  | (delta == 1 || delta == 11) && rootDiffers = Just Leading
  | (delta == 1 || delta == 11) && not rootDiffers = Just Parallel
  | otherwise = Nothing

describeTransform :: Triad -> Triad -> Maybe TriadicTransform
describeTransform t1 t2 =
  let delta = findChanged t1 t2
      (r1, _, _) = t1
      (r2, _, _) = t2
      rootDiffers = r1 /= r2
   in do
        d <- delta
        makeTxform d rootDiffers

third :: (a, b, c) -> c
third (_, _, c) = c

matchTriadicTransform :: TriadicTransform -> G.LEdge TriadEdgeLabel -> Bool
matchTriadicTransform tx edge = third edge == tx

-- makePath :: TriadicTransform -> TriadNodeLabel -> Maybe TriadNodeLabel
makePath :: TriadicTransform -> TriadNodeLabel -> TriadNodeLabel
makePath tx origin =
  let originIdx = nodeLookup Map.! origin
      edges = G.out triadGraph originIdx
      matchingEdges = filter (matchTriadicTransform tx) edges
      matchingNodes = map (\(from, to, label) -> nodeLookup' Map.! to) matchingEdges
   in head matchingNodes

--   in listToMaybe matchingNodes

-- the Parallel transformation exchanges a triad for its Parallel. 
-- In a Major Triad move the third down a semitone (C major to C minor), 
-- in a Minor Triad move the third up a semitone (C minor to C major)
-- transformation moves the root note up or down a half step
p :: TriadNodeLabel -> TriadNodeLabel
p = makePath Parallel


-- The Relative ransformation exchanges a triad for its Relative. 
-- In a Major Triad move the fifth up a tone (C major to A minor), 
-- in a Minor Triad move the root down a tone (A minor to C major)
r :: TriadNodeLabel -> TriadNodeLabel
r = makePath Relative

-- the Leading tone transformation exchanges a triad for its Leading-Tone Exchange. 
-- In a Major Triad the root moves down by a semitone (C major to E minor), 
-- in a Minor Triad the fifth moves up by a semitone (E minor to C major)
l :: TriadNodeLabel -> TriadNodeLabel
l = makePath Leading

-- starting from a major chord: magical
-- starting from a minor chord: sinister
-- magical1 / sinister
lp :: TriadNodeLabel -> TriadNodeLabel
lp = l >>> p

-- starting from a major chord: magical
-- starting from a minor chord: sinister
-- magical2 / sinister2
pl :: TriadNodeLabel -> TriadNodeLabel
pl = p >>> l

-- heroic1 / uncanny
pr :: TriadNodeLabel -> TriadNodeLabel
pr = p >>> r

-- heroic2 / uncanny
rp :: TriadNodeLabel -> TriadNodeLabel
rp = r >>> p

prl :: TriadNodeLabel -> TriadNodeLabel
prl = p >>> r >>> l

slide :: TriadNodeLabel -> TriadNodeLabel
slide = l >>> p >>> r

hexapole :: TriadNodeLabel -> TriadNodeLabel
hexapole = l >>> p >>> l

nebenverwandt :: TriadNodeLabel -> TriadNodeLabel
nebenverwandt = r >>> l >>> p

type TriadTraversal = TriadNodeLabel -> TriadNodeLabel

findChordProgression :: TriadNodeLabel -> [TriadTraversal] -> [TriadNodeLabel]
findChordProgression start [] = []
findChordProgression start (hd : tl) =
  let next = hd start
   in next : findChordProgression next tl

printFlat :: [[String]] -> IO ()
printFlat ns = mapM_ putStrLn (concat ns)

transforms :: [TriadNodeLabel -> TriadNodeLabel]
transforms = [p, r, l, slide, lp, pl, pr, rp, hexapole, prl, nebenverwandt]

transformNames :: [String]
transformNames = ["p", "r", "l", "slide", "lp", "pl", "pr", "rp", "hexapole", "prl", "nebenverwandt"]
