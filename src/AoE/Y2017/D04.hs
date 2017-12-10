module AoE.Y2017.D04 where

import qualified Data.Set as Set
import Data.List (sort)

validPassword :: String -> Bool
validPassword password = length ws == Set.size (Set.fromList ws)
    where ws = words password

solve :: [String] -> Int
solve = length . filter validPassword

validPassword2 :: String -> Bool
validPassword2 password = length ws == Set.size (Set.fromList ws)
    where ws = map sort . words $ password

solve2 :: [String] -> Int
solve2 = length . filter validPassword2

main :: IO ()
main = do
    xs <- fmap lines getContents
    print $ solve xs
    print $ solve2 xs
