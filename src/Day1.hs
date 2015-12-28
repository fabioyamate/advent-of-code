{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import Data.List (foldl')

moveFloor acc c
  | c == '(' = acc + 1
  | c == ')' = acc - 1
  | otherwise = acc

part1 :: [Char] -> Int
part1 = foldl' moveFloor 0

part2 :: [Char] -> Int
part2 = length . takeWhile (>= 0) . scanl moveFloor 0

-- part2 c acc

main = do
  root <- getCurrentDirectory
  moves <- readFile $ root </> "dist/resources/day1.txt"
  print $ part1 moves
  print $ part2 moves
