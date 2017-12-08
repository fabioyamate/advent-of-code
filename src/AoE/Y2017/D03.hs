{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D03 where

import Control.Arrow ((&&&), (>>>), arr)
import Data.List (sort)

-- builds a collection of steps per layer
f :: Int -> [(Int,Int)]
f x | x == 0 = [(0,0)]
    | odd x = (replicate x (1,0)) ++ (replicate x (0,1))
    | even x = (replicate x (-1,0)) ++ (replicate x (0,-1))

sumVector ((x,y),_) (x',y') = ((x+x',y+y'), abs (x+x') + abs (y+y'))

spiralDirections = [v | x <- [1..], v <- f x]

-- the list comprehension builds the spiral vector
-- then we store into a structure the position and the distance, which is distance between
-- the origin to the vector (p,q) == |p| + |q|
solve :: Int -> Int
solve p = snd $ (!! (p-1)) $ scanl sumVector ((0,0), 0) spiralDirections

solve2 :: Int -> Int
solve2 = undefined

-- l = 0
-- 1 = [0,0]
-- l = 1
-- 2 = [1,0] -> [1,0]
-- 3 = [0,1] -> [1,1]
-- l = 2
-- 4 = [-1,0] -> [0,1]
-- 5 = [-1,0] -> [-1,1]
-- 6 = [0,-1] -> [-1,0]
-- 7 = [0,-1] -> [-1,-1]
-- l = 3 (odd = positive)
-- 8 = [1,0] -> [0,-1]
-- 9 = [1,0] -> [1,-1]
-- 10 = [1,0] -> [2,-1]
-- 11 = [0,1] -> [2,0]
-- 12 = [0,1] -> [2,1]
-- 13 = [0,1] -> [2,2]
-- l = 4 (even = negative)
-- 14 = [-1,0] -> [1,2]
-- 15 = [-1,0] -> [0,2]
-- 16 = [-1,0] -> [-1,2]
-- 17 = [-1,0] -> [-2,2]
-- 18 = [0,-1] -> [-2,1]
-- 19 = [0,-1] -> [-2,0]
-- 20 = [0,-1] -> [-2,-1]
-- 21 = [0,-1] -> [-2,-2]
-- l = 5
-- 22 = [1,0] -> [-1,-2]
-- 23 = [1,0] -> [0,-2]
-- 24 = [1,0] -> [1,-2]
-- 25 = [1,0] -> [2,-2]
-- 26 = [1,0] -> [3,-2]
-- 27 = [0,1] -> [3,-1]
-- 28 = [0,1] -> [3,0]
-- 29 = [0,1] -> [3,1]
-- 30 = [0,1] -> [3,2]
-- 31 = [0,1] -> [3,3]
-- l = 6
-- ...

main :: IO ()
main = do
    print $ solve 1
    print $ solve 12
    print $ solve 23
    print $ solve 1024
    print $ solve 277678
