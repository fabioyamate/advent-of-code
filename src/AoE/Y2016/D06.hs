{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D06 where

import Data.List (transpose, group, sort, maximumBy, minimumBy, length)
import Control.Arrow ((&&&), (>>>), (***))
import Data.Ord (comparing)

frequency = map (head &&& length) . group . sort

mostCommon = fst . maximumBy (comparing snd) . frequency

leastCommon = fst . minimumBy (comparing snd) . frequency

solution1 = map mostCommon . transpose

solution2 = map leastCommon . transpose

main :: IO ()
main = do
     contents <- getContents
     print . solution2 . lines $ contents
