{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D06 where

-- 16 memory banks
-- N blocks per bank
-- reallocation = balance blocks
-- even balanced are tied by the lower index
-- moves blocks to the next index bank
-- divmod the blocks

import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

firstMaximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
firstMaximumBy cmp = foldl1 max'
    where max' x y = case cmp x y of
                        LT -> y
                        _  -> x

dump :: [(Int, Int)] -> String
dump xs = traceId $ unwords . map show . map snd $ xs

reallocate :: [(Int, Int)] -> [(Int, Int)]
reallocate xs = map (\(idx, n) -> if idx == i then (idx, q) else (idx, n + d)) xs
    where (i, n) = firstMaximumBy (comparing snd) xs
          (d, q) = n `divMod` (length xs - 1)

parse :: String -> [(Int,Int)]
parse = zip [0..] . map read . words

debug :: [(Int,Int)] -> Set String -> Int -> Int
debug xs s clocks = if Set.member (dump snapshot) s
                        then clocks + 1
                        else debug snapshot (Set.insert (dump snapshot) s) (clocks + 1)
    where snapshot = reallocate xs

solve :: [(Int,Int)] -> Int
solve xs = debug xs Set.empty 0

main :: IO ()
main = do
    xs <- fmap parse getContents
    print $ xs
    print $ solve xs
