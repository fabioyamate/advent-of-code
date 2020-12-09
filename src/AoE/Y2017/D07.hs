{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D07 where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import Data.List (nub, null, sort, group, sortBy, length)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Either (lefts)

type Disk = (String, (Int, [String]))

skipSpaces = skipWhile isSpace

parseName :: Parser String
parseName = many1 letter_ascii

parseChilds :: Parser [String]
parseChilds = do
  " -> "
  names <- parseName `sepBy` (char ',' *> skipSpaces)
  return names

parseDisk :: Parser Disk
parseDisk = do
  name <- parseName
  skipSpaces
  char '('
  weight <- decimal
  char ')'
  childs <- option [] parseChilds
  return (name, (weight, childs))

parseInput :: Parser [Disk]
parseInput = many' $ parseDisk <* (endOfInput <|> endOfLine)

frequencies xs = map (\vs -> (head vs, length vs)) . group . sort $ xs

-- word frequency
solve xs = fst . head . sortBy (comparing snd) . frequencies . concatMap (\(name,(_,children)) -> name:children) $ xs

diffWeight [] = 0
diffWeight xs = maximum xs - minimum xs

weight :: Map.Map String (Int, [String]) -> String -> Either Int (Int, Int)
weight table name = if null children
                        then Right (w, 0)
                        else case ws of
                            Right ys -> if length (nub (map (uncurry (+)) ys)) == 1
                                then Right (w, sum ys)
                                else Left (w - diffWeight ys)
                            Left err -> Left err
    where (w, children) = fromJust $ Map.lookup name table
          ws = foldl (\acc x -> (:) <$> (weight table x) <*> acc) (Right []) children

-- find the node where childs are unbalanced
solve2 input = head . lefts . map (weight table) $ (Map.keys table)
    where table = Map.fromList input 

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly parseInput contents of
                 Right disks -> solve2 $ disks
