module VigenereEncode where

import Data.Char (ord, chr)


vigenereEncode :: String -> String -> String
vigenereEncode _ [] = []
vigenereEncode (t:ts) (x:xs)
	| ord 'a' <= ord x && ord 'z' >= ord x = (chr $ (ord t + ord x - ord 'a') `mod` 26 + ord 'a') : vigenereEncode ts xs
    | ord 'A' <= ord x && ord 'Z' >= ord x = (chr $ (ord t + ord x - ord 'A') `mod` 26 + ord 'A') : vigenereEncode ts xs
    | True = x : vigenereEncode ts xs