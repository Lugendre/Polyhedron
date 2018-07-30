module Polyhedron where

import Numeric.LinearAlgebra

-- Data Types

type PolyHed = [Vector R]
type Vectors = [Vector R]


-- Implementation

polyArea4 :: Vectors -> R
polyArea4 (r1:r2:r3:_) = area r1 r2 + area r2 r3 + area r3 r1 + area r4 r5
    where
        r4 = r1 - r2
        r5 = r2 - r3
        area :: Vector R -> Vector R -> R
        area ri rj = (norm_2  $ cross ri rj) / 2

polyVol4 :: Vectors -> R
polyVol4 = (/6) . det . fromColumns






-- Helper Function

genVectors :: PolyHed -> Vectors
genVectors [] = []
genVectors (p1:ps) = map (subtract p1) ps
