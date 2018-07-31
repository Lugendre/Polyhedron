module Delaunay where

import Numeric.LinearAlgebra
import Data.List
import qualified Data.Map as Map

-- Data Types

data Circle = Circle (Vector R) R deriving (Show)

type Triangle = [Vector R]

type Tetrahedron = [Vector R]

type TetraMap = Map.Map Tetrahedron Bool

type Polyhedron = [Vector R]


-- Helper

hasCommonPoints :: Tetrahedron -> Tetrahedron -> Bool
hasCommonPoints th1 th2
    | (is == []) = False
    | otherwise = True
        where
            is = th1 `intersect` th2

getCircumcircle :: Tetrahedron -> Circle
getCircumcircle (r1:th1) = Circle cp rad
    where
        cm = fromRows (map (`subtract` r1) th1) * 2
        dm = reshape 1 $ vector $ map (dot iv) $ map (`subtract` r2) th2
        r2 = cmap (^2) r1
        th2 =  map (cmap (^2)) $ th1
        cp = flatten $ luSolve (luPacked cm) dm
        rad = norm_2 (cp - r1)
        iv = vector [1, 1, 1]

addTetrahedron :: Tetrahedron -> TetraMap -> TetraMap
addTetrahedron th ths
    | (Map.member th ths) = Map.adjust f th ths
    | otherwise = Map.insert th True ths
        where
            f _ = False

getHugeTetrahedron :: Polyhedron -> Tetrahedron
getHugeTetrahedron ph = undefined


p1 = vector [1,2,3]
p2 = vector [7,4,2]
p3 = vector [5,5,9]
p4 = vector [0,0,0]
th1 = [p1,p2,p3,p4]

p5 = vector [1,0,0]
p6 = vector [0,1,0]
p7 = vector [0,0,1]
p8 = vector [0,0,0]
th2 = [p5,p6,p7,p8]

thm = Map.fromList [(th1, True), (th2, True)]

rads (Circle cp r) = map norm_1 $ map (subtract cp) th1
