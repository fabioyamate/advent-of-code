{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2017.D05 where

import Control.Arrow ((&&&), (>>>), arr)
import Data.Vector.Unboxed (Vector, (!), (!?), (//))
import qualified Data.Vector.Unboxed as Vector
import Data.Maybe (isNothing)
import Debug.Trace

-- (state, instructions, position)
type Program = (Vector Int, Int, Int)

finished :: Program -> Bool
finished (v, _, pos) = isNothing (v !? pos)

run :: Program -> Int
run program@(v,i,p) = if finished program
    then i
    else run (v // [(p, (v ! p) + 1)], -- increment current offset by 1
              i + 1,                   -- increment instruction
              p + (v ! p))             -- move position by offset

solve :: Vector Int -> Int
solve v = run (v, 0, 0)

updateOffset :: Program -> Int
updateOffset (v,i,p) = if (v ! p) >= 3 then (v ! p) - 1 else (v ! p) + 1

run2 :: Program -> Int
run2 program@(v,i,p) = if finished program
    then i
    else run2 (v // [(p, updateOffset program)], -- increment current offset by 1
               i + 1,                            -- increment instruction
               p + (v ! p))                      -- move position by offset

solve2 :: Vector Int -> Int
solve2 v = run2 (v, 0, 0)

parse :: String -> Vector Int
parse = Vector.fromList . map read . lines

main :: IO ()
main = do
    xs <- fmap parse getContents
    print $ solve xs
    -- print $ solve2 xs -- too slow, WIP
