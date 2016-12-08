{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D04 where

import Control.Applicative ((<|>))
import Data.Text as T
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import Data.List (sort)
import Prelude hiding (takeWhile)

data Room = Room { _roomName :: Text
                 , _roomId :: Int
                 , _roomChecksum :: String
                 } deriving Show

parseRoomName :: Parser BS.ByteString
parseRoomName = takeWhile (inClass "a-z-")

parseChecksum :: Parser String
parseChecksum = do
  char '['
  checksum <- many' letter_ascii
  char ']'
  return checksum

parseRoom :: Parser Room
-- parseRoom = undefined
parseRoom = do
  name <- parseRoomName
  roomId <- decimal
  checksum <- parseChecksum
  return (Room (T.unpack name) roomId checksum)

main :: IO ()
main = do
     contents <- BS.getContents
     print $ case parseOnly parseRoom contents of
                  Right directions -> directions
