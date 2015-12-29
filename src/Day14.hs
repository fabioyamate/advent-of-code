{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import qualified Data.ByteString as BS
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 hiding (take, transpose)
import Data.List (transpose)

type Name = String
type Speed = Int
type Fly = Int
type Rest = Int
data Reindeer = Reindeer Name Speed Fly Rest deriving Show

word :: Parser [Char]
word = many1 letter_ascii

parseReindeer :: Parser Reindeer
parseReindeer = do
    name <- word
    string " can fly "
    speed <- decimal
    string " km/s for "
    fly <- decimal
    string " seconds, but then must rest for "
    rest <- decimal
    string " seconds."
    return $ Reindeer name speed fly rest

parseInput :: Parser [Reindeer]
parseInput = many' $ parseReindeer <* (endOfInput <|> endOfLine)

velocity :: Reindeer -> [Int]
velocity (Reindeer _ speed fly rest) = cycle $ (replicate fly speed) ++ (replicate rest 0)

distance :: (Num a) => Int -> Reindeer -> Int
distance seconds reinder = sum . take seconds . velocity $ reinder

part1 :: Int -> [Reindeer] -> Int
part1 seconds reindeers = maximum . map (distance seconds) $ reindeers

pointsGiven :: [Int] -> [Int]
pointsGiven xs = map points xs
   where max = maximum xs
         points x = if x == max then 1 else 0 

part2 :: Int -> [Reindeer] -> Int
part2 seconds reindeers = maximum . totalPoints seconds . pointsWon $ reindeers
    where positions = drop 1 . scanl (+) 0 . velocity              -- compute snapshots of position for each second
          pointsWon = map pointsGiven . transpose . map positions  -- give points to leaders for each second
          totalPoints seconds = map sum . transpose . take seconds -- sum points in each second

main = do
    contents <- BS.getContents
    print $ case parseOnly parseInput contents of
                Right reindeers -> (part1 2503 reindeers, part2 2503 reindeers)
