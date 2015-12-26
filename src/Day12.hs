{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM

part1 acc (Number a) = acc + a
part1 acc (Array v) = foldl part1 acc v
part1 acc (Object m) = foldl part1 acc m
part1 acc _ = acc

part2 acc (Number a) = acc + a
part2 acc (Array v) = foldl part2 acc v
part2 acc (Object m) = if (HM.member "red" m) || elem "red" values
                       then acc
                       else foldl part2 acc m
    where values = HM.elems m
          
part2 acc _ = acc

main = do
    contents <- B.getContents
    print $ part1 0 $ fromJust (decode contents :: Maybe Value)
    print $ part2 0 $ fromJust (decode contents :: Maybe Value)
