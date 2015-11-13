module VigenereDecode where

import Data.Char (ord, chr)

vigenereDecode :: String -> String -> String
vigenereDecode _ [] = []
vigenereDecode (t:ts) (x:xs)
	| ord 'a' <= ord x && ord 'z' >= ord x = (chr $ (-ord t + ord x - ord 'a') `mod` 26 + ord 'a') : vigenereDecode ts xs
	| ord 'A' <= ord x && ord 'Z' >= ord x = (chr $ (-ord t + ord x - ord 'A') `mod` 26 + ord 'A') : vigenereDecode ts xs
	| True = x : vigenereDecode ts xs
