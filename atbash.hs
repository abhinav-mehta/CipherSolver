import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

replacel::[Char] -> [Char]
replacel [] = []
replacel (x:xs) |x=='a' = ('z':(replacel  xs))
				 |x=='b' = ('y':(replacel  xs))
				 |x=='c' = ('x':(replacel  xs))
				 |x=='d' = ('w':(replacel  xs))
				 |x=='e' = ('v':(replacel  xs))
				 |x=='f' = ('u':(replacel  xs))
				 |x=='g' = ('t':(replacel  xs))
				 |x=='h' = ('s':(replacel  xs))
				 |x=='i' = ('r':(replacel  xs))
				 |x=='j' = ('q':(replacel  xs))
				 |x=='k' = ('p':(replacel  xs))
				 |x=='l' = ('o':(replacel  xs))
				 |x=='m' = ('n':(replacel  xs))
				 |x=='n' = ('m':(replacel  xs))
				 |x=='o' = ('l':(replacel  xs))
				 |x=='p' = ('k':(replacel  xs))
				 |x=='q' = ('j':(replacel  xs))
				 |x=='r' = ('i':(replacel  xs))
				 |x=='s' = ('h':(replacel  xs))
				 |x=='t' = ('g':(replacel  xs))
				 |x=='u' = ('f':(replacel  xs))
				 |x=='v' = ('e':(replacel  xs))
				 |x=='w' = ('d':(replacel  xs))
				 |x=='x' = ('c':(replacel  xs))
				 |x=='y' = ('b':(replacel  xs))
				 |x=='z' = ('a':(replacel  xs))
				 |otherwise = (x:(replacel  xs))

main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	let cipher = replacel samecasewords
	print cipher