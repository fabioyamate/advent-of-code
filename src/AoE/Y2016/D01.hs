{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D01 where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8

type Coordinate = (Int, Int)

data Move = L Int | R Int deriving Show

data Direction = North Coordinate
               | South Coordinate
               | East Coordinate
               | West Coordinate
               deriving Show

parseMove :: Parser Move
parseMove = do
      (char 'L' >> decimal >>= (return . L))
  <|> (char 'R' >> decimal >>= (return . R))

changeDirection :: Direction -> Move -> Direction
changeDirection (North (x,y)) (L s) = West (x-s,y)
changeDirection (North (x,y)) (R s) = East (x+s,y)
changeDirection (South (x,y)) (L s) = East (x+s,y)
changeDirection (South (x,y)) (R s) = West (x-s,y)
changeDirection (East (x,y)) (L s) = North (x,y+s)
changeDirection (East (x,y)) (R s) = South (x,y-s)
changeDirection (West (x,y)) (L s) = South (x,y-s)
changeDirection (West (x,y)) (R s) = North (x,y+s)

blocksAway (North (x,y)) = (abs x) + (abs y)
blocksAway (South (x,y)) = (abs x) + (abs y)
blocksAway (East (x,y)) = (abs x) + (abs y)
blocksAway (West (x,y)) = (abs x) + (abs y)

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly (parseMove `sepBy` (string ", ")) contents of
                  Right moves -> blocksAway $ foldl changeDirection (North (0,0)) moves
