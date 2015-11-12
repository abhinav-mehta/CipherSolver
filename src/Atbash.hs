import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

atbash::[Char] -> [Char]
atbash [] = []
atbash (x:xs) |x=='a' = ('z':(atbash  xs))
				 |x=='b' = ('y':(atbash  xs))
				 |x=='c' = ('x':(atbash  xs))
				 |x=='d' = ('w':(atbash  xs))
				 |x=='e' = ('v':(atbash  xs))
				 |x=='f' = ('u':(atbash  xs))
				 |x=='g' = ('t':(atbash  xs))
				 |x=='h' = ('s':(atbash  xs))
				 |x=='i' = ('r':(atbash  xs))
				 |x=='j' = ('q':(atbash  xs))
				 |x=='k' = ('p':(atbash  xs))
				 |x=='l' = ('o':(atbash  xs))
				 |x=='m' = ('n':(atbash  xs))
				 |x=='n' = ('m':(atbash  xs))
				 |x=='o' = ('l':(atbash  xs))
				 |x=='p' = ('k':(atbash  xs))
				 |x=='q' = ('j':(atbash  xs))
				 |x=='r' = ('i':(atbash  xs))
				 |x=='s' = ('h':(atbash  xs))
				 |x=='t' = ('g':(atbash  xs))
				 |x=='u' = ('f':(atbash  xs))
				 |x=='v' = ('e':(atbash  xs))
				 |x=='w' = ('d':(atbash  xs))
				 |x=='x' = ('c':(atbash  xs))
				 |x=='y' = ('b':(atbash  xs))
				 |x=='z' = ('a':(atbash  xs))
				 |otherwise = (x:(atbash  xs))

{-
main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	let cipher = atbash samecasewords
	print cipher
-}
