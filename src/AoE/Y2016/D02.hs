{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D02 where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8

type Coordinate = (Int, Int)

data Direction = DU | DD | DL | DR deriving Show

parseDirection :: Parser Direction
parseDirection = do
      (char 'U' >> return DU)
  <|> (char 'D' >> return DD)
  <|> (char 'R' >> return DR)
  <|> (char 'L' >> return DL)

parseDirections = many' $ (many1 parseDirection) <* (endOfInput <|> endOfLine)

move :: Coordinate -> Direction -> Coordinate
move (x,y) direction =
  case direction of
    DU -> if (abs (y+1) > 1) then (x,y) else (x,y+1)
    DD -> if (abs (y-1) > 1) then (x,y) else (x,y-1)
    DR -> if (abs (x+1) > 1) then (x,y) else (x+1,y)
    DL -> if (abs (x-1) > 1) then (x,y) else (x-1,y)

h :: [[Direction]] -> [Int]
h xxs = drop 1 . map g $ scanl f (0,0) xxs
  where f pos xs = foldl move pos xs
        g (x,y) = 5 + x - 3*y

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly parseDirections contents of
                  Right directions -> h directions
