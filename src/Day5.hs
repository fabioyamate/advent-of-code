{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import qualified Data.Map as Map

isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

threeVowels = (== 3) . B.length . B.take 3 . B.filter isVowel

doubleLetter = isJust . L.find (>= 2) . map B.length . B.group

notAllowedSubstring input = not $ any (\subs -> B.isInfixOf subs input) ["ab", "cd", "pq", "xy"]

niceWordRule xs = (threeVowels xs) && (doubleLetter xs) && (notAllowedSubstring xs)

-- part 2

frequencies :: (Num a, Ord k) => [k] -> Map.Map k a
frequencies xs = Map.fromListWith (+) [(x,1) | x <- xs]

slidingBuffer :: Int -> [a] -> [[a]]
slidingBuffer _ [] = []
slidingBuffer _ [_] = []
slidingBuffer n xs = [take n xs] ++ slidingBuffer n (tail xs)

aroundLetter :: String -> Bool
aroundLetter xs = or [L.isInfixOf sub xs | sub <- combinations] 
    where combinations = [[x, y, x] | x <- ['a'..'z'], y <- ['a'..'z']]

repeatsWithoutOverlap :: String -> String -> Bool
repeatsWithoutOverlap xs ys = not . and $ [overlap x y | x <- positions, y <- positions, x /= y]
   where positions = L.elemIndices ys (slidingBuffer 2 xs)
         overlap x y = abs (y - x) == 1

condition1 :: String -> Bool
condition1 xs = any (repeatsWithoutOverlap xs) pairs
    where letters = Map.keys . Map.filter (> 1) . frequencies $ xs
          pairs = [[x, y] | x <- letters, y <- letters]

niceWord2Rule :: String -> Bool
niceWord2Rule xs = condition1 xs && aroundLetter xs

main :: IO ()
main = do
     contents <- B.getContents
     print $ length . filter niceWordRule . B.lines $ contents
     print $ length . filter niceWord2Rule . map B.unpack . B.lines $ contents