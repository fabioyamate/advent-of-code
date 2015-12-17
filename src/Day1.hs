{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))

floor c acc
  | c == '(' = acc + 1
  | c == ')' = acc - 1
  | otherwise = acc

main = do
  root <- getCurrentDirectory
  moves <- readFile $ root </> "dist/resources/day1.txt"
  print $ foldr floor 0 moves
