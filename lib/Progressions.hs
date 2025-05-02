module Progressions where

import NeoRiemann

data Progression = I | II | III | IV | V | VI | VII deriving Enum

makeProgression :: Triad -> Progression -> Mood -> Triad
makeProgression triad@(Triad r t f _) progression mood = let scalePattern = case mood of
                                                                Minor -> minor
                                                                Major -> major
                                                             scale = makeScale scalePattern r
                                                             indx = fromEnum progression
                                                             newRoot = scale !! indx
                                                             newTriad = case mood of 
                                                                Major -> makeMajorTriad newRoot
                                                                Minor -> makeMinorTriad newRoot
                                                          in newTriad
