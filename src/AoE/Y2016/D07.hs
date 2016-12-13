{-# LANGUAGE FlexibleContexts #-}

module AoE.Y2016.D07 where

-- import Control.Applicative ((<|>))
import Text.Parsec
import Text.Parsec.Char (endOfLine)
import Data.List (length)

data IPv7 = IPv7 { _a :: String
                 , _hypernet :: String
                 , _b :: String
                 } deriving Show

parseIPv7 :: Stream s m Char => ParsecT s u m IPv7
parseIPv7 = do
  a <- many1 letter
  char '['
  hypernet <- many1 letter
  char ']'
  b <- many1 letter
  return $ IPv7 a hypernet b

supportsTLS :: IPv7 -> Bool
supportsTLS ip = False

main :: IO ()
main = do
     contents <- getContents
     print $ case parse (many1 (parseIPv7 <* endOfLine)) "" contents of
                  Right ips -> length [ip | ip <- ips, supportsTLS ip]
