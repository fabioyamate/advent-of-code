module Day20 where

import Data.Set as S
import Math.NumberTheory.Primes.Factorisation

-- basically the elves that deliveries presents in house N are
-- the factors(N), for example given 15 = 15 5 3 1, these are
-- the elves number. Then the presents are:
--
-- presents(N) = 10*sum(factors(N))
--
-- the naive solution would work but for large numbers of N it
-- takes long time to run
--
-- we need heuristics to reduce the space of numbers to test
--
-- 1. factors are multiple of prime numbers

-- factors :: Int -> [Int]
-- factors n = n : [x | x <- [1..max], mod n x == 0]
--    where max = n `div` 2

-- presents :: Int -> Int
-- presents n = 10*sum(factors(n))

house p = head [x | x <- [1..], presents'(x) >= threshold]
    where presents' n = sum(divisors(n))
          threshold = p `div` 10

part2 k p = head [x | x <- [1..], presents'(x) >= threshold]
    where factors n = S.filter (\x -> x*k >= n) . divisors $ n
          presents' n = sum(factors(n))
          threshold = p `div` 10

f a b = S.filter (\x -> x * b >= a) . divisors $ a

main = do
    -- print $ factors 15
    -- print $ presents 15
    -- print $ house 36000000
    -- print $ part2 36000000
    -- 20790000
    -- 36000000
    -- print $ divisors 100
    -- print $ S.filter (\x -> x*10 >= 48) . divisors $ 48
    print $ take 10 $ [(part2 10 x) | x <- [1..]]
    --print $ divisors 942480
    --print $ S.filter (\x -> x*50 >= 942480) . divisors $ 942480
    --print $ sum $ S.filter (\x -> x*50 >= 942480) . divisors $ 942480
    -- print $ part2 50 36000000
    -- print $ sum $ S.filter (\x -> x*50 >= 100) . divisors $ 100
    -- print $ part2 60
