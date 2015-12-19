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

area :: Dimension -> Int
area (Dimension l w h) = 2 * (sum faceAreas) + minimum faceAreas
  where faceAreas = [l*w, w*h, l*h]

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly parseDimensions contents of
                  Right dimensions -> sum $ map area dimensions
