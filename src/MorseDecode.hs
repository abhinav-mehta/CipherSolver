import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

morseDecode::[String] -> String
morseDecode [] = []
morseDecode (x:xs) |x==".-" = ('a':(morseDecode  xs))
				 |x=="-..." = ('b':(morseDecode  xs))
				 |x=="-.-." = ('c':(morseDecode  xs))
				 |x=="-.." = ('d':(morseDecode  xs))
				 |x=="." = ('e':(morseDecode  xs))
				 |x=="..-." = ('f':(morseDecode  xs))
				 |x=="--." = ('g':(morseDecode  xs))
				 |x=="...." = ('h':(morseDecode  xs))
				 |x==".." = ('i':(morseDecode  xs))
				 |x==".---" = ('j':(morseDecode  xs))
				 |x=="-.-" = ('k':(morseDecode  xs))
				 |x==".-.." = ('l':(morseDecode  xs))
				 |x=="--" = ('m':(morseDecode  xs))
				 |x=="-." = ('n':(morseDecode  xs))
				 |x=="---" = ('o':(morseDecode  xs))
				 |x==".--." = ('p':(morseDecode  xs))
				 |x=="--.-" = ('q':(morseDecode  xs))
				 |x==".-." = ('r':(morseDecode  xs))
				 |x=="..." = ('s':(morseDecode  xs))
				 |x=="-" = ('t':(morseDecode  xs))
				 |x=="..-" = ('u':(morseDecode  xs))
				 |x=="...-" = ('v':(morseDecode  xs))
				 |x==".--" = ('w':(morseDecode  xs))
				 |x=="-..-" = ('x':(morseDecode  xs))
				 |x=="-.--" = ('y':(morseDecode  xs))
				 |x=="--.." = ('z':(morseDecode  xs))
				 |x=="-----" = ('0':(morseDecode  xs))
				 |x==".---" = ('1':(morseDecode  xs))
				 |x=="..---" = ('2':(morseDecode  xs))
				 |x=="...--" = ('3':(morseDecode  xs))
				 |x=="....-" = ('4':(morseDecode  xs))
				 |x=="....." = ('5':(morseDecode  xs))
				 |x=="-...." = ('6':(morseDecode  xs))
				 |x=="--..." = ('7':(morseDecode  xs))
				 |x=="---.." = ('8':(morseDecode  xs))
				 |x=="----." = ('9':(morseDecode  xs))
				 |x==".-.-.-" = ('.':(morseDecode  xs))
				 |x=="--..--" = (',':(morseDecode  xs))
				 |x=="..--.." = ('?':(morseDecode  xs))
				 |x=="-....-" = ('-':(morseDecode  xs))
				 |x=="-...-" = ('=':(morseDecode  xs))
				 |x=="---..." = (':':(morseDecode  xs))
				 |x=="-.-.-." = (';':(morseDecode  xs))
				 |x=="-.--." = ('(':(morseDecode  xs))
				 |x=="-.--.-" = (')':(morseDecode  xs))
				 |x=="-..-." = ('/':(morseDecode  xs))
				 |x==".-..-." = ('"':(morseDecode  xs))
				 |x=="...-..-" = ('$':(morseDecode  xs))
				 |x==".----." = ('\'':(morseDecode  xs))
				 |x=="..--.-" = ('_':(morseDecode  xs))
				 |x==".--.-." = ('@':(morseDecode  xs))
				 |x=="..--." = ('!':(morseDecode  xs))
				 |x==".-.-." = ('+':(morseDecode  xs))
				 |x==".-..." = ('~':(morseDecode  xs))
				 |x=="...-.-" = ('#':(morseDecode  xs))
				 |x==".-..." = ('&':(morseDecode  xs))
				 |x=="/" = (' ':(morseDecode  xs))


{-
main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	let final = words samecasewords
	let cipher = morseDecode final
	print cipher
-}