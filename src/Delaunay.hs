module Delaunay where

import Numeric.LinearAlgebra
import Data.List

-- Data Types

data Circle = Circle (Vector R) R deriving (Show)

type Triangle = [Vector R]

type Tetrahedron = [Vector R]



-- Helper

hasCommonPoints :: Tetrahedron -> Tetrahedron -> Bool
hasCommonPoints th1 th2 = sort th1 == sort th2

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

p1 = vector [1,2,3]
p2 = vector [7,4,2]
p3 = vector [5,5,9]
p4 = vector [0,0,0]
th = [p1,p2,p3,p4]

rads (Circle cp r) = map norm_1 $ map (subtract cp) th
