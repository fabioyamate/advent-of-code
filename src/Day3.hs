{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import qualified Data.Set as S
import Data.List (foldl')

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

part1 :: [Direction] -> Int
part1 = countUnique . deliveries initialPosition

partitionBy2 [] = []
partitionBy2 [x] = []
partitionBy2 (x:y:xs) = (x,y) : (partitionBy2 xs)

moveBoth (p1,p2,houses) (d1,d2) = (p1',p2',houses')
    where (p1',p2') = (move d1 p1, move d2 p2)
          houses' = S.insert p1' . S.insert p2' $ houses

part2 :: [Direction] -> Int
part2 moves = S.size houses
    where splittedMoves = partitionBy2 moves
          (_,_,houses) = foldl' moveBoth ((0,0),(0,0),S.singleton (0,0)) splittedMoves

main :: IO ()
main = do
    directions <- getContents
    print $ part1 directions
    print $ part2 directions