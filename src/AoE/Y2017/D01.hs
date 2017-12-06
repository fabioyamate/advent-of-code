{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D01 where

import Data.List (take, splitAt)
import Data.Char (digitToInt, isDigit)

f :: [Int] -> [Int] -> [Int]
f [] _ = []
f _ [] = []
f (x:xs) (y:ys) = if x == y
    then (x:(f xs ys))
    else (0:(f xs ys))

solve :: String -> Int
solve xs = sum $ take n $ f xxs (tail xxs)
    where n = length xs
          xxs = cycle (map digitToInt xs)

solve2 :: String -> Int
solve2 xs = sum $ take n $ f xxs (drop (n `div` 2) xxs)
    where n = length xs
          xxs = cycle (map digitToInt xs)

main :: IO ()
main = do
    print $ solve "1122"
    print $ solve "1111"
    print $ solve2 "1111"

    print $ solve "123"
    print $ solve "912362789"
    contents <- getContents
    print $ solve (takeWhile isDigit contents)
    print $ solve2 (takeWhile isDigit contents)
