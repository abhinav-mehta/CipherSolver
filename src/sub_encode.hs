import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

replacel::String -> [Char] -> [Char]
replacel y [] = []
replacel [] x = x
replacel y (x:xs) |x=='a' = ((y !! 0):(replacel y xs))
				 |x=='b' = ((y !! 1):(replacel y xs))
				 |x=='c' = ((y !! 2):(replacel y xs))
				 |x=='d' = ((y !! 3):(replacel y xs))
				 |x=='e' = ((y !! 4):(replacel y xs))
				 |x=='f' = ((y !! 5):(replacel y xs))
				 |x=='g' = ((y !! 6):(replacel y xs))
				 |x=='h' = ((y !! 7):(replacel y xs))
				 |x=='i' = ((y !! 8):(replacel y xs))
				 |x=='j' = ((y !! 9):(replacel y xs))
				 |x=='k' = ((y !! 10):(replacel y xs))
				 |x=='l' = ((y !! 11):(replacel y xs))
				 |x=='m' = ((y !! 12):(replacel y xs))
				 |x=='n' = ((y !! 13):(replacel y xs))
				 |x=='o' = ((y !! 14):(replacel y xs))
				 |x=='p' = ((y !! 15):(replacel y xs))
				 |x=='q' = ((y !! 16):(replacel y xs))
				 |x=='r' = ((y !! 17):(replacel y xs))
				 |x=='s' = ((y !! 18):(replacel y xs))
				 |x=='t' = ((y !! 19):(replacel y xs))
				 |x=='u' = ((y !! 20):(replacel y xs))
				 |x=='v' = ((y !! 21):(replacel y xs))
				 |x=='w' = ((y !! 22):(replacel y xs))
				 |x=='x' = ((y !! 23):(replacel y xs))
				 |x=='y' = ((y !! 24):(replacel y xs))
				 |x=='z' = ((y !! 25):(replacel y xs))
				 |otherwise = (x:(replacel y xs))

main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	keyf <- openFile "key.txt" ReadMode
	key <- hGetContents keyf
	let cipher = replacel key samecasewords
	print cipher

