module Day15 where

import qualified Data.Matrix as M

-- naive approach with combinatorial solution, complexity increases with matrix dimension

-- Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
-- Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9
-- Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1
-- Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8

m = M.fromLists [[3,0,0,-3]
                ,[-3,3,0,0]
                ,[-1,0,4,0]
                ,[0,0,-2,2]]

solution x y z t = M.fromList 4 1 [x,y,z,t]

negativeToZero x = if x < 0 then 0 else x

f x y z t = foldl (*) 1 $ map negativeToZero $ M.toList $ M.multStd2 (M.transpose m) (solution x y z t)

-- it tests 10^8 combinations, where 170k are valid

-- possible heuristics, the constant multiplied by a positive must be greater than the negative ones

part1 = maximum [(f x y z t) | x <- [0..100],
                               y <- [0..100],
                               z <- [0..100],
                               t <- [0..100],
                               x + y + z + t == 100]

part2 = maximum [(f x y z t) | x <- [0..100],
                               y <- [0..100],
                               z <- [0..100],
                               t <- [0..100],
                               x + y + z + t == 100,
                               x*2+9*y+z+8*t == 500]

main :: IO ()
main = do
    print $ part1
    print $ part2