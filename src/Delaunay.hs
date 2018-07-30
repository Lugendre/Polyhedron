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
        cm = tr $ fromColumns (map (subtract r1) th1) * 2
        dm = tr $ fromColumns (map (subtract r2) th2)
        r2 = cmap (^2) r1
        th2 =  map (cmap (^2)) $ th1
        cp = flatten $ luSolve (luPacked cm) dm
        rad = norm_1 (cp - r1)

