module Day8 where

import Data.List
import Control.Monad.IO.Class

type Parser = [(Char -> Bool)]
type State = (Int,Int)
type ParseResult = (Bool, Int, Int, String)

match :: Parser -> String -> Bool
match [] _ = True
match _ [] = False
match (m:ms) (y:ys) = if not (m y)
                      then False
                      else (match ms ys)

isHex :: Char -> Bool
isHex c = c == '0'
          || c == '1'
          || c == '2'
          || c == '3'
          || c == '4'
          || c == '5'
          || c == '6'
          || c == '7'
          || c == '8'
          || c == '9'
          || c == 'a'
          || c == 'b'
          || c == 'c'
          || c == 'd'
          || c == 'e'
          || c == 'f'

parserEscapedQuote :: Parser
parserEscapedQuote = [(== '\\'), (== '"')]

parserBackslash :: Parser
parserBackslash = [(== '\\'), (== '\\')]

parserHex :: Parser
parserHex = [(== '\\'), (== 'x'), isHex, isHex]

parserQuote :: Parser
parserQuote = [(== '"')]

parserAny :: Parser
parserAny = [const True]

matchWithSize :: Parser -> String -> State -> ParseResult 
matchWithSize parser xs (sSize,bSize) = if match parser xs
                                        then (True,sSize + parserSize, bSize + 1, drop parserSize xs)
                                        else (False,sSize,bSize,xs)
   where parserSize = length parser

matchNoCount :: Parser -> String -> State -> ParseResult
-- consumes char, but it is not a string
matchNoCount parser xs (sSize,bSize) = if match parser xs
                                       then (True,sSize + parserSize, bSize, drop parserSize xs)
                                       else (False,sSize,bSize,xs)
    where parserSize = length parser

-- naive approach, it probably can be refactored with maybe monad
-- fail case are not threated properly, just ignored assuming that the input is valid
parseSpecialChar xs state = case (matchWithSize parserEscapedQuote xs state) of
    ok@(True,_,_,_) -> ok
    (False,_,_,_) -> case (matchWithSize parserBackslash xs state) of
                                 ok@(True,_,_,_) -> ok
                                 (False,_,_,_) -> case (matchWithSize parserHex xs state) of
                                   ok@(True,_,_,_) -> ok
                                   fail@(False,_,_,_) -> case (matchNoCount parserQuote xs state) of
                                     ok@(True,_,_,_) -> ok
                                     fail@(False,_,_,_) -> case (matchWithSize parserAny xs state) of
                                       ok@(True,_,_,_) -> ok
                                       fail -> fail

consume :: String -> State -> State
consume [] state = state
consume xs state = consume xs' (sSize,bSize)
    where (_,sSize,bSize,xs') = parseSpecialChar xs state

encode [] = []
encode (x:xs) = if (x == '"' || x == '\\')
              then '\\' : x : (encode xs)
              else x : (encode xs)

encodeString xs = "\"" ++ (encode xs) ++ "\""

part1 strings = sSize - bSize
    where (sSize,bSize) = foldl' (flip consume) (0,0) strings

part2 strings = sSize' - sSize
    where encodedStrings = map encodeString strings
          (sSize,_) = foldl' (flip consume) (0,0) strings
          (sSize',_) = foldl' (flip consume) (0,0) encodedStrings

main = do
    contents <- getContents
    print $ part1 . lines $ contents
    print $ part2 . lines $ contents
