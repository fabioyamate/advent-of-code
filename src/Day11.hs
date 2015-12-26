module Day11 where

import Data.Char
import Data.List

-- (carrier,result) :: (Int,Int)

succLetter c = if c == 'z'
               then 'a'
               else chr (ord c + 1)

plusChar c carrier = (c', carrier')
    where c' = if carrier == 1 then succLetter c else c
          carrier' = if c' < c then 1 else 0

f x (acc,carrier) = if x + carrier > (ord 'z')
                    then ((ord 'a'):acc, 1)
                    else ((x + carrier) : acc, 0)

next = fst . foldr f ([],1)

-- this is a naive implementation that does not apply any heuristics to the computation
-- some improvements would be for any i, l or o, we just reset the following chars with 'a' since
-- all substring with this chars should be ignored
nextCombination xs = map chr offsets
    where offsets = next $ map ord xs

nextPassword xs = validPasswords !! 0
    where passwords = drop 1 . iterate nextCombination $ xs
          validPasswords = filter validPassword passwords

charDistance (x:y:xs) = ord y - ord x

consecutive xs = all (== 1) distances
    where groups = partitionBy 2 1 xs
          distances = map charDistance groups

validPassword :: String -> Bool
validPassword xs = cond1 xs && cond2 xs && cond3 xs
    where cond1 = any consecutive . partitionBy 3 1
          cond2 = not . any (\x -> x == 'i' || x == 'l' || x == 'o')
          cond3 = (>= 2) . length . filter (> 1) . map length . group

partitionBy n step xs
    | length xs < n = []
    | otherwise = take n xs : (partitionBy n step (drop step xs))

main :: IO ()
main = do
    print $ nextPassword "abcdefgh"
    print $ nextPassword "cqjxjnds"
    print $ nextPassword "cqjxxyzz"
