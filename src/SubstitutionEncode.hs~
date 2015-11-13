module SubstitutionEncode where

import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

substitutionEncode::String -> [Char] -> [Char]
substitutionEncode y [] = []
substitutionEncode [] x = x
substitutionEncode y (x:xs) |x=='a' = ((y !! 0):(substitutionEncode y xs))
				 |x=='b' = ((y !! 1):(substitutionEncode y xs))
				 |x=='c' = ((y !! 2):(substitutionEncode y xs))
				 |x=='d' = ((y !! 3):(substitutionEncode y xs))
				 |x=='e' = ((y !! 4):(substitutionEncode y xs))
				 |x=='f' = ((y !! 5):(substitutionEncode y xs))
				 |x=='g' = ((y !! 6):(substitutionEncode y xs))
				 |x=='h' = ((y !! 7):(substitutionEncode y xs))
				 |x=='i' = ((y !! 8):(substitutionEncode y xs))
				 |x=='j' = ((y !! 9):(substitutionEncode y xs))
				 |x=='k' = ((y !! 10):(substitutionEncode y xs))
				 |x=='l' = ((y !! 11):(substitutionEncode y xs))
				 |x=='m' = ((y !! 12):(substitutionEncode y xs))
				 |x=='n' = ((y !! 13):(substitutionEncode y xs))
				 |x=='o' = ((y !! 14):(substitutionEncode y xs))
				 |x=='p' = ((y !! 15):(substitutionEncode y xs))
				 |x=='q' = ((y !! 16):(substitutionEncode y xs))
				 |x=='r' = ((y !! 17):(substitutionEncode y xs))
				 |x=='s' = ((y !! 18):(substitutionEncode y xs))
				 |x=='t' = ((y !! 19):(substitutionEncode y xs))
				 |x=='u' = ((y !! 20):(substitutionEncode y xs))
				 |x=='v' = ((y !! 21):(substitutionEncode y xs))
				 |x=='w' = ((y !! 22):(substitutionEncode y xs))
				 |x=='x' = ((y !! 23):(substitutionEncode y xs))
				 |x=='y' = ((y !! 24):(substitutionEncode y xs))
				 |x=='z' = ((y !! 25):(substitutionEncode y xs))
				 |otherwise = (x:(substitutionEncode y xs))

{-
main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	keyf <- openFile "key.txt" ReadMode
	key <- hGetContents keyf
	let cipher = substitutionEncode key samecasewords
	print cipher
-}
