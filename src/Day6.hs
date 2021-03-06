{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as Map
import qualified Data.List as L

-- turn on 0,0 through 999,999
-- toggle 0,0 through 999,0
-- turn off 499,499 through 500,500

-- word word coordinate word coordinate
-- word coordinate word coordinate

data Coordinate = Coordinate Int Int deriving Show
data Area = Area Coordinate Coordinate deriving Show

data Action = TurnOn | TurnOff | Toggle deriving Show

data Command = Command Action Area deriving Show
type Instructions = [Command]

type Light = (Int, Int)
data LightState = On | Off deriving Show

-- naive approach with Map
type Grid = Map.Map Light LightState

-- Grammar

parseCoordinate :: Parser Coordinate
parseCoordinate = do
    x <- decimal
    char ','
    y <- decimal
    return $ Coordinate x y

parseArea :: Parser Area
parseArea = do
    from <- parseCoordinate
    space
    string "through"
    space
    to <- parseCoordinate
    return $ Area from to

parseAction :: Parser Action
parseAction = do
        (string "turn on" >> return TurnOn)
    <|> (string "turn off" >> return TurnOff)
    <|> (string "toggle" >> return Toggle)

parseCommand :: Parser Command
parseCommand = do
    action <- parseAction
    space
    area <- parseArea
    return $ Command action area

parseInstructions :: Parser Instructions
parseInstructions = many' $ parseCommand <* (endOfInput <|> endOfLine)

-- code

toggle (Just On) = Nothing
toggle Nothing = Just On

turnOn _ = Just On
turnOff _ = Nothing

lightsOn :: Grid -> Int
lightsOn = length

areaLights (Area (Coordinate fromX fromY) (Coordinate toX toY)) = [(x,y) | x <- [fromX..toX], y <- [fromY..toY]]

action TurnOn = turnOn
action TurnOff = turnOff
action Toggle = toggle

execute (Command a area) lights = L.foldl' update lights selection
    where update = flip $ Map.alter (action a)
          selection = areaLights area

part1 :: Instructions -> Int
part1 = lightsOn . L.foldl' (flip execute) Map.empty

-- part2

changeBrightness f brightness = if newBrightness < 0 then Just 0 else Just newBrightness
    where newBrightness = f brightness

increaseBy x n = n + x
decreaseBy x n = n - x

brightness TurnOn = changeBrightness (increaseBy 1)
brightness TurnOff = changeBrightness (decreaseBy 1)
brightness Toggle = changeBrightness (increaseBy 2)

initBrightness :: Map.Map (Int, Int) Int
initBrightness = Map.fromList [((x,y), 0) | x <- [0..999], y <- [0..999]]

executeBrightness (Command a area) lights = L.foldl' update lights selection
    where update = flip $ Map.update (brightness a)
          selection = areaLights area

totalBrightness :: Map.Map (Int, Int) Int -> Int
totalBrightness = Map.foldl' (+) 0

part2 :: Instructions -> Int
part2 = totalBrightness . L.foldl' (flip executeBrightness) initBrightness

main = do
    contents <- BS.getContents
    print $ case parseOnly parseInstructions contents of
         Right instructions -> part2 instructions
