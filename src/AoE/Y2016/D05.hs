{-# LANGUAGE OverloadedStrings #-}

module AoE.Y2016.D05 where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B

md5 = Hex.encode . MD5.hash

attempt k n = B.append k (B.pack (show n))

found k n = (== "00000") . B.take 5 . md5 $ attempt k n

-- mine secretKey number = if found secretKey Number
--                         then md5 attempt
--                         else mine secretKey (number + 1)


main = do
    -- print $ mine "abbhdwsy" 0
    let k = "abbhdwsy"
    print $ map (\x -> x !! 5) $ take 8 [B.unpack $ md5 (attempt k n) | n <- [0..], found k n]

