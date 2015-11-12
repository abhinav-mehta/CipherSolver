module Main where

import System.IO
import Data.List
import Data.Char
import Data.Tuple
import CaesarEncode
import CaesarDecode

{-
main :: IO()
main = do
	data1 <- hGetContents =<< openFile "american-english" ReadMode
	data2 <- hGetContents =<< openFile "cracklib-small" ReadMode
	let bigData = sort $ (words $ map cleaner data1) ++ (words $ map cleaner data2)
	raw <-getLine
	let raw2 = words $ map cleaner raw
	putStrLn (solveCaesar bigData raw2)
-}

solveCaesar :: [String] -> [String] -> String
solveCaesar bigData raw = caesarDecode t2 $ unwords raw 
	where
		t1 = fmax $ matchNumber [0, 1.. 25] bigData raw 
			where
				fmax k = maximum (map fst k)   
		t2 = sfmax (matchNumber [0, 1.. 25] bigData raw) t1   
			where	
				sfmax [] _ = 0
				sfmax (x:xs) t1 | fst x == t1 = snd x
								| True = sfmax xs t1
				

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



