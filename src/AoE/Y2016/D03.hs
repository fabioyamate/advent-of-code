{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D03 where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import Data.List (sort)

type Sides = (Int, Int, Int)

skipSpaces = skipWhile isSpace

parseSides :: Parser Sides
parseSides = do
  skipSpaces
  a <- decimal
  skipSpaces
  b <- decimal
  skipSpaces
  c <- decimal
  return (a,b,c)

parseAllSides = many' $ parseSides <* (endOfInput <|> endOfLine)

isTriangle (a,b,c) = a + b > c && a + c > b && b + c > a

solution1 directions = length $ filter isTriangle directions

solution2 [] = []
solution2 ((a1,a2,a3):(b1,b2,b3):(c1,c2,c3):ys) = (a1,b1,c1):(a2,b2,c2):(a3,b3,c3):(solution2 ys)

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly parseAllSides contents of
                  Right directions -> solution1 $ solution2 directions
