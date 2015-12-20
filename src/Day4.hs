{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B

md5 input = Hex.encode . MD5.hash

found = (== "000000") . B.take 6 . Hex.encode . MD5.hash

mine secretKey number = if found attempt
                        then number
                        else mine secretKey (number + 1)
     where attempt = B.append secretKey (B.pack (show number))

main = do
     print $ mine "bgvyzdsv" 1
