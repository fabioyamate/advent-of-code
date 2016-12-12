{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D06 where

import Data.List (transpose, group, sort, maximumBy, length)
import Control.Arrow ((&&&), (>>>), (***))
import Data.Ord (comparing)

higherFrequency = fst . maximumBy (comparing snd) . map (head &&& length) . group . sort

main :: IO ()
main = do
     contents <- getContents
     print . map higherFrequency . transpose . lines $ contents
