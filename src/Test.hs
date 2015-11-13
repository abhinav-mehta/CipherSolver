module Test where

import System.IO
import System.Random
import System.IO.Unsafe
import Data.List
import Data.Char
import Data.Tuple

import AffineDecode
import AffineEncode
import Atbash
import CaesarEncode
import CaesarDecode
import MorseEncode
import MorseDecode
--import RSAEncode
--import SubstitutionEncode
import VigenereEncode
import VigenereDecode
--import PlayFair

repeater :: String -> String
repeater t = t ++ repeater t

cleaner :: Char -> Char
cleaner x 
	| isAlpha x || x=='\'' = x
	| True = ' '	

randomStr :: Int -> String
randomStr t = take t $ randomRs ('a','z') $ unsafePerformIO newStdGen

randomNs :: Int -> [Int]
randomNs t = take t $ randomRs (1,1000) $ unsafePerformIO newStdGen

main = do
	data1 <- hGetContents =<< openFile "dict/american-english" ReadMode
	data2 <- hGetContents =<< openFile "dict/cracklib-small" ReadMode
	let bigData = sort $ (words $ map cleaner data1) ++ (words $ map cleaner data2)
	
	putStrLn "AFFINE"
	let numberA = head $ randomNs 1
	let numberB = head $ randomNs 1
	print numberA  
	print numberB 
	let myAffineString = randomStr 10
	putStrLn myAffineString	 
	putStrLn "AFFINE ENCODE"
	let myAffineEncode = affineEncode numberA numberB myAffineString
	putStrLn $ myAffineEncode
	putStrLn "AFFINE DECODE"
	let myAffineDecode = affineDecode numberA numberB myAffineEncode
	putStrLn $ myAffineDecode
	
	putStrLn "ATBASH"
	--let numberA = head $ randomNs 1
	--let numberB = head $ randomNs 1
	--print numberA  
	--print numberB 
	let myAtbashString = randomStr 10
	putStrLn myAtbashString	 
	putStrLn "ATBASH ENCODE"
	let myAtbashEncode = atbash myAtbashString
	putStrLn $ myAtbashEncode
	putStrLn "AFFINE DECODE"
	let myAtbashDecode = atbash myAtbashEncode
	putStrLn $ myAtbashDecode
	
	putStrLn "CAESAR"
	let numberC = head $ randomNs 1
	--let numberB = head $ randomNs 1
	print numberC  
	--print numberB 
	let myCaesarString = randomStr 10
	putStrLn myCaesarString	 
	putStrLn "CAESAR ENCODE"
	let myCaesarEncode = caesarEncode numberC myCaesarString
	putStrLn $ myCaesarEncode
	putStrLn "CAESAR DECODE"
	let myCaesarDecode = caesarDecode numberC myCaesarEncode
	putStrLn $ myCaesarDecode

	putStrLn "CAESAR"
	let numberC = head $ randomNs 1
	--let numberB = head $ randomNs 1
	print numberC  
	--print numberB 
	let myCaesarString = randomStr 10
	putStrLn myCaesarString	 
	putStrLn "CAESAR ENCODE"
	let myCaesarEncode = caesarEncode numberC myCaesarString
	putStrLn $ myCaesarEncode
	putStrLn "CAESAR DECODE"
	let myCaesarDecode = caesarDecode numberC myCaesarEncode
	putStrLn $ myCaesarDecode

	putStrLn "MORSE"
	--let numberC = head $ randomNs 1
	--let numberB = head $ randomNs 1
	--print numberC  
	--print numberB 
	let myMorseString = randomStr 10
	putStrLn myMorseString	 
	putStrLn "MORSE ENCODE"
	let myMorseEncode = morseEncode myMorseString
	putStrLn $ myMorseEncode
	putStrLn "MORSE DECODE"
	let myMorseDecode = morseDecode $ words myMorseEncode
	putStrLn $ myMorseDecode

	putStrLn "VIGENERE"
	let rkey = randomStr 3
	--let numberB = head $ randomNs 1
	putStrLn rkey
	let key = repeater rkey  
	--print numberB 
	let myVigenereString = randomStr 10
	putStrLn myVigenereString	 
	putStrLn "VIGENERE ENCODE"
	let myVigenereEncode = vigenereEncode key myVigenereString
	putStrLn $ myCaesarEncode
	putStrLn "VIGENERE DECODE"
	let myVigenereDecode = vigenereDecode key myVigenereEncode
	putStrLn $ myVigenereDecode	

	

	







{-
main = do
     putStrLn randomStr
     putStrLn randomStr


	raw <-getLine
	let raw2 = words $ map cleaner raw
	putStrLn (decrypt bigData raw2)
	putStrLn "Abhinav"
-}