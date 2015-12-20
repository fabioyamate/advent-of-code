{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)

isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

threeVowels = (== 3) . B.length . B.take 3 . B.filter isVowel

doubleLetter = isJust . L.find (>= 2) . map B.length . B.group

notAllowedSubstring input = not $ any (\subs -> B.isInfixOf subs input) ["ab", "cd", "pq", "xy"]

niceWordRule xs = (threeVowels xs) && (doubleLetter xs) && (notAllowedSubstring xs)

main = do
     contents <- B.getContents
     print $ length . filter niceWordRule . B.lines $ contents
