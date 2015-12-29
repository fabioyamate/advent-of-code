{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8

type Length = Int
type Width = Int
type Height = Int

data Dimension = Dimension Length Width Height deriving Show

parseDimension :: Parser Dimension
parseDimension = do
  length <- decimal
  char 'x'
  width <- decimal
  char 'x'
  height <- decimal
  return $ Dimension length width height

parseDimensions :: Parser [Dimension]
parseDimensions = many' $ parseDimension <* (endOfInput <|> endOfLine)

combine2 :: [a] -> [[a]]
combine2 [] = []
combine2 (x:xs) = (combine' x xs) ++ (combine2 xs)
    where combine' _ [] = []
          combine' x (y:ys) = [x,y] : (combine' x ys)

area :: Dimension -> Int
area (Dimension l w h) = 2 * (sum faceAreas) + minimum faceAreas
  where faceAreas = [l*w, w*h, l*h]

ribbon :: Dimension -> Int
ribbon (Dimension l w h) = w*l*h + minimum facePerimeters
     where facePerimeters = [2*(w+l), 2*(w+h), 2*(l+h)]

part1 :: [Dimension] -> Int
part1 = sum . map area

part2 :: [Dimension] -> Int
part2 = sum . map ribbon

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly parseDimensions contents of
                  Right dimensions -> (part1 dimensions, part2 dimensions)
