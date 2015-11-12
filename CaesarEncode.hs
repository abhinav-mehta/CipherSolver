module CaesarEncode where

import Data.Char (ord, chr)

caesarEncode :: Int -> String -> String
caesarEncode t = map f
 	where f c
      		| ord 'a' <= ord c && ord 'z' >= ord c = chr $ (t + ord c - ord 'a') `mod` 26 + ord 'a'
      		| ord 'A' <= ord c && ord 'Z' >= ord c = chr $ (t + ord c - ord 'A') `mod` 26 + ord 'A'
      		| True = c
