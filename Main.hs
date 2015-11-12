module Main where

import System.IO
import Data.List
import Data.Char
import Data.Tuple
import CaesarEncode
import CaesarDecode


main :: IO()
main = do
	data1 <- hGetContents =<< openFile "american-english" ReadMode
	data2 <- hGetContents =<< openFile "cracklib-small" ReadMode
	let bigData = sort $ (words $ map cleaner data1) ++ (words $ map cleaner data2)
	raw <-getLine
	let raw2 = words raw
	rotate <- getLine
	print bigData


decrypt :: [String] -> [String] -> String
decrypt bigData raw = caesarDecode t $ unwords raw 
	where
		t = snd $ cmax $ matchNumber [0...25] bigData raw  
		where	
			cmax (x:xs) = fst x : max 


matchNumber :: [Int] -> [String] -> [String] -> [(Int,Int)]
matchNumber [] _ _ = []
matchNumber (t:ts) bigData raw = (sum $ map (match t bigData) raw, t) : matchNumber ts bigData raw


match :: Int -> [String] -> String -> Int
match t bigData raw 
	| decrypted `elem` bigData = 1
	| True = 0
		where 
			decrypted = caesarDecode t raw


cleaner :: Char -> Char
cleaner x 
	| isAlpha x || x=='\'' = x
	| True = ' '	



