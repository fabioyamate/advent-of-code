module Day17 where

import Data.List (sort, group)

combine1 _ 0 = 1
combine1 [] _ = 0
combine1 (x:xs) max
   | max' == 0 = 1 + combine'
   | max' < 0 = combine'
   | otherwise = (combine1 xs max') + combine'
   where max' = max - x
         combine' = if (sum xs) < max' then 0 else (combine1 xs max)

-- the number of elements is the height of the recursion
combine2 _ 0 h = [h]
combine2 [] _ h = []
combine2 (x:xs) max h
   | max' == 0 = [h'] ++ combine'
   | max' < 0 = [] ++ combine'
   | otherwise = (combine2 xs max' h') ++ combine'
   where max' = max - x
         h' = h + 1
         combine' = if (sum xs) < max' then [] else (combine2 xs max h)

-- this probably could fit to a mapreduce approach
-- where you first map all height with count 1 (height, count) and then reduce to accumulate the minimum with count
-- given (h,c) and (h',c'), if h > h' then keep (h',c')
--                             h == h' then (h, c+c')
--                             h < h' then (h,c)

sample = [20,15,10,5,5]
containers = [11,30,47,31,32,36,3,1,5,3,32,36,15,11,46,26,28,1,19,3]

main = do
    print $ combine1 sample 25
    print $ combine1 containers 150
    print $ minimum $ combine2 sample 25 0
    print $ length . head . group . sort $ combine2 containers 150 0