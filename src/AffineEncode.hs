module AffineEncode where

import System.IO
import Data.List
import Data.Char (ord, chr)


affineEncode :: Int -> Int -> String -> String
affineEncode a b = map f
 	where f c = chr $ (a*(ord c - ord 'a')+b) `mod` 26 + ord 'a'

{-
affineDecode :: Int -> Int -> String -> String
affineDecode a b = map f
 	where f c = chr $ (26 - a `mod` 26)*(ord c - ord 'A' - b) `mod` 26 + ord 'A'
-}
{-
main :: IO()
main = do 
	putStrLn $ affineDecode 5 8 $ affineEncode 5 8 "AFFINECIPHER"
	data1 <- hGetContents =<< openFile "american-english" ReadMode
	data2 <- hGetContents =<< openFile "cracklib-small" ReadMode
	let bigData = sort $ (words data1) ++ (words data2)
	print bigData
-}	
