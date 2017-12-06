{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D02 where

import Control.Arrow ((&&&), (>>>), arr)

-- quick way to get primes
import Math.NumberTheory.Primes.Sieve (primes)

diff :: [Integer] -> Integer
diff = maximum &&& minimum >>> uncurry (-)

solve :: [String] -> Integer
solve = sum . map diff . parse

divisibleBy n x = n `mod` x == 0

divisibleBetween xs = all (`divisibleBy` m) xs
    where m = minimum xs

diff2 :: [Integer] -> Integer
diff2 xs = diff' $ head $ filter divisibleBetween ys 
   where diff' = maximum &&& minimum >>> uncurry div
         ys = [divisibles | p <- primes, let divisibles = filter (`divisibleBy` p) xs, length divisibles == 2]

solve2 :: [String] -> Integer
solve2 = sum . map diff2 . parse

parse :: [String] -> [[Integer]]
parse = map (map read . words)

main :: IO ()
main = do
    xs <- fmap lines getContents
    print $ solve xs
    print $ solve2 xs
