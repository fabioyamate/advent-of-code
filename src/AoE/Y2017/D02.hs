{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D02 where

import Control.Arrow ((&&&), (>>>), arr)
import Data.List (sort)

diff :: [Integer] -> Integer
diff = maximum &&& minimum >>> uncurry (-)

solve :: [String] -> Integer
solve = sum . map diff . parse

divisibleBy n x = n `mod` x == 0

divisibleBetween xs = all (`divisibleBy` m) xs
    where m = minimum xs


diff2 :: [Integer] -> Integer
-- rationale, there will be list of numbers
-- if there are 2 numbers divisible, if we
-- test from minimum to maximum number we will
-- quickly find the first disible number
--
-- permutation would be another option, which is close
-- to what is done here
diff2 = head . g . sort
    where g [] = []
          g (x:ys) = [diff | y <- ys, let (diff, m) = y `divMod` x, m == 0] ++ (g ys)

solve2 :: [String] -> Integer
solve2 = sum . map diff2 . parse

parse :: [String] -> [[Integer]]
parse = map (map read . words)

main :: IO ()
main = do
    xs <- fmap lines getContents
    print $ solve xs
    print $ solve2 xs
