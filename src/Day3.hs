{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import qualified Data.Set as S

type Position = (Int, Int)
type Direction = Char
type Path = String
type Houses = [Position]

move :: Direction -> Position -> Position
move direction position@(x, y)
  | direction == '^' = (x, y + 1)
  | direction == '>' = (x + 1, y)
  | direction == 'v' = (x, y - 1)
  | direction == '<' = (x - 1, y)
  | otherwise = position

initialPosition :: Position
initialPosition = (0,0)

deliveries :: Position -> Path -> Houses
deliveries position path = scanr move position path

countUnique :: (Eq a, Ord a) => [a] -> Int
countUnique = S.size . S.fromList

main :: IO ()
main = do
     directions <- getContents
     print $ countUnique $ deliveries initialPosition directions