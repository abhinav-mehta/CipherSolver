import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

replacel::[String] -> String
replacel [] = []
replacel (x:xs) |x==".-" = ('a':(replacel  xs))
				 |x=="-..." = ('b':(replacel  xs))
				 |x=="-.-." = ('c':(replacel  xs))
				 |x=="-.." = ('d':(replacel  xs))
				 |x=="." = ('e':(replacel  xs))
				 |x=="..-." = ('f':(replacel  xs))
				 |x=="--." = ('g':(replacel  xs))
				 |x=="...." = ('h':(replacel  xs))
				 |x==".." = ('i':(replacel  xs))
				 |x==".---" = ('j':(replacel  xs))
				 |x=="-.-" = ('k':(replacel  xs))
				 |x==".-.." = ('l':(replacel  xs))
				 |x=="--" = ('m':(replacel  xs))
				 |x=="-." = ('n':(replacel  xs))
				 |x=="---" = ('o':(replacel  xs))
				 |x==".--." = ('p':(replacel  xs))
				 |x=="--.-" = ('q':(replacel  xs))
				 |x==".-." = ('r':(replacel  xs))
				 |x=="..." = ('s':(replacel  xs))
				 |x=="-" = ('t':(replacel  xs))
				 |x=="..-" = ('u':(replacel  xs))
				 |x=="...-" = ('v':(replacel  xs))
				 |x==".--" = ('w':(replacel  xs))
				 |x=="-..-" = ('x':(replacel  xs))
				 |x=="-.--" = ('y':(replacel  xs))
				 |x=="--.." = ('z':(replacel  xs))
				 |x=="-----" = ('0':(replacel  xs))
				 |x==".---" = ('1':(replacel  xs))
				 |x=="..---" = ('2':(replacel  xs))
				 |x=="...--" = ('3':(replacel  xs))
				 |x=="....-" = ('4':(replacel  xs))
				 |x=="....." = ('5':(replacel  xs))
				 |x=="-...." = ('6':(replacel  xs))
				 |x=="--..." = ('7':(replacel  xs))
				 |x=="---.." = ('8':(replacel  xs))
				 |x=="----." = ('9':(replacel  xs))
				 |x==".-.-.-" = ('.':(replacel  xs))
				 |x=="--..--" = (',':(replacel  xs))
				 |x=="..--.." = ('?':(replacel  xs))
				 |x=="-....-" = ('-':(replacel  xs))
				 |x=="-...-" = ('=':(replacel  xs))
				 |x=="---..." = (':':(replacel  xs))
				 |x=="-.-.-." = (';':(replacel  xs))
				 |x=="-.--." = ('(':(replacel  xs))
				 |x=="-.--.-" = (')':(replacel  xs))
				 |x=="-..-." = ('/':(replacel  xs))
				 |x==".-..-." = ('"':(replacel  xs))
				 |x=="...-..-" = ('$':(replacel  xs))
				 |x==".----." = ('\'':(replacel  xs))
				 |x=="..--.-" = ('_':(replacel  xs))
				 |x==".--.-." = ('@':(replacel  xs))
				 |x=="..--." = ('!':(replacel  xs))
				 |x==".-.-." = ('+':(replacel  xs))
				 |x==".-..." = ('~':(replacel  xs))
				 |x=="...-.-" = ('#':(replacel  xs))
				 |x==".-..." = ('&':(replacel  xs))
				 |x=="/" = (' ':(replacel  xs))

main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	let final = words samecasewords
	let cipher = replacel final
	print cipher