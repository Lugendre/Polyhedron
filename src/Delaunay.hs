module Delaunay where

import Numeric.LinearAlgebra
import Data.List

-- Data Types

data Circle = Circle R R deriving (Show)

type Triangle = [Vector R]

type Tetrahedron = [Vector R]



-- Helper

hasCommonPoints :: Tetrahedron -> Tetrahedron -> Bool
hasCommonPoints th1 th2 = sort th1 == sort th2

getCircumcircle :: Tetrahedron -> Circle
getCircumcircle (r1:th1) = luSolve (luPacked cm) dm
    where
        cm = 2 * tr fromColumns (map (subtract r1) th1)
        dm = tr fromColumns (map (subtract r2) th2)
        r2 = cmap (^2) r1
        th2 = cmap (^2) th2


