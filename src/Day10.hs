module Day10 where

import Data.List (group)

lookAndSay [] = []
lookAndSay all@(x:xs) = show (length all) ++ [x]

doLookAndSay s 0 = s
doLookAndSay s n = doLookAndSay s' (n - 1)
    where s' = concat . map lookAndSay . group $ s

main = do
    print $ length $ doLookAndSay "1113122113" 40
    print $ length $ doLookAndSay "1113122113" 50
