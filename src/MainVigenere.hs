module Main2 where

import System.IO
import Data.List
import Data.Char
import Data.Tuple
import CaesarEncode
import CaesarDecode

englishData = [
					('A',8.167), ('B',1.492), ('C',2.782), ('D',4.253), ('E',12.702),
                    ('F',2.228), ('G',2.015), ('H',6.094), ('I',6.996), ('J',0.153),
                    ('K',0.772), ('L',4.025), ('M',2.406), ('N',6.749), ('O',7.507),
                    ('P',1.929), ('Q',0.095), ('R',5.987), ('S',6.327), ('T',9.056),
                    ('U',2.758), ('V',0.978), ('W',2.360), ('X',0.150), ('Y',1.974),
                    ('Z',0.074), 
                    ('a',8.167), ('b',1.492), ('c',2.782), ('d',4.253), ('e',12.702),
                    ('f',2.228), ('g',2.015), ('h',6.094), ('i',6.996), ('j',0.153),
                    ('k',0.772), ('l',4.025), ('m',2.406), ('n',6.749), ('o',7.507),
                    ('p',1.929), ('q',0.095), ('r',5.987), ('s',6.327), ('t',9.056),
                    ('u',2.758), ('v',0.978), ('w',2.360), ('x',0.150), ('y',1.974),
                    ('z',0.074) 
                ]


main :: IO()
main = do
	data1 <- hGetContents =<< openFile "american-english" ReadMode
	data2 <- hGetContents =<< openFile "cracklib-small" ReadMode
	let bigData = sort $ (words $ map cleaner data1) ++ (words $ map cleaner data2)
	raw <-getLine
	let raw2 = words $ map cleaner raw
	putStrLn (decrypt bigData raw2)



decrypt :: [String] -> [String] -> String
decrypt bigData raw = caesarDecode t2 $ unwords raw 
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



