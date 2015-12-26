{-# LANGUAGE OverloadedStrings #-}

module Day9 where

-- This is a travel Salesman problem

import qualified Data.ByteString as BS
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (delete, nub)

type City = String
type Distance = Int
type Path = (City,City,Distance)
type Graph = Map.Map (City,City) Distance

sample = [("London", "Dublin", 464)
         ,("London", "Belfast", 518)
         ,("Dublin", "Belfast", 141)]

-- grammar

parseCity :: Parser City
parseCity = many1 letter_ascii

parsePath :: Parser Path
parsePath = do
    from <- parseCity
    string " to "
    to <- parseCity
    string " = "
    distance <- decimal
    return $ (from,to,distance)

parsePaths :: Parser [Path]
parsePaths = many' $ parsePath <* (endOfInput <|> endOfLine)

buildGraph = Map.fromList . concat . map pairs 
    where pairs (a,b,d) = [((a,b),d), ((b,a),d)]

allCities :: [Path] -> [City]
allCities = nub . concat . map (\(x,y,_) -> [x,y])

-- pick one point as start, and try to visit all cities
-- cities ["London", "Dublin", "Belfast"]

findDistance :: (City,City) -> Graph -> Distance
findDistance k graph = fromJust $ Map.lookup k graph

shortestDistance :: Graph -> City -> [City] -> Distance -> Distance
shortestDistance _ _ [] distance = distance
shortestDistance graph city cities distance = minimum distances
    where distances = map (\x -> shortestDistance graph x (delete x cities) (distance + (findDistance (city, x) graph))) cities


longestDistance :: Graph -> City -> [City] -> Distance -> Distance
longestDistance _ _ [] distance = distance
longestDistance graph city cities distance = maximum distances
    where distances = map (\x -> longestDistance graph x (delete x cities) (distance + (findDistance (city, x) graph))) cities

compute f graph cities city = f graph city remainingCities 0
    where remainingCities = delete city cities

distances f paths = map (compute f graph cities) cities
    where graph = buildGraph paths
          cities = allCities paths

part1 = minimum . distances shortestDistance
part2 = maximum . distances longestDistance

main = do
    contents <- BS.getContents
    print $ case parseOnly parsePaths contents of
       Right paths -> (part1 paths, part2 paths)
