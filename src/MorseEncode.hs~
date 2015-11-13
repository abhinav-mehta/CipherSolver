module MorseEncode where

import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

morseEncode::[Char] -> String
morseEncode [] = []
morseEncode (x:xs) |x=='a' = (".- "++(morseEncode  xs))
				 |x=='b' = ("-... "++(morseEncode  xs))
				 |x=='c' = ("-.-. "++(morseEncode  xs))
				 |x=='d' = ("-.. "++(morseEncode  xs))
				 |x=='e' = (". "++(morseEncode  xs))
				 |x=='f' = ("..-. "++(morseEncode  xs))
				 |x=='g' = ("--. "++(morseEncode  xs))
				 |x=='h' = (".... "++(morseEncode  xs))
				 |x=='i' = (".. "++(morseEncode  xs))
				 |x=='j' = (".--- "++(morseEncode  xs))
				 |x=='k' = ("-.- "++(morseEncode  xs))
				 |x=='l' = (".-.. "++(morseEncode  xs))
				 |x=='m' = ("-- "++(morseEncode  xs))
				 |x=='n' = ("-. "++(morseEncode  xs))
				 |x=='o' = ("--- "++(morseEncode  xs))
				 |x=='p' = (".--. "++(morseEncode  xs))
				 |x=='q' = ("--.- "++(morseEncode  xs))
				 |x=='r' = (".-. "++(morseEncode  xs))
				 |x=='s' = ("... "++(morseEncode  xs))
				 |x=='t' = ("- "++(morseEncode  xs))
				 |x=='u' = ("..- "++(morseEncode  xs))
				 |x=='v' = ("...- "++(morseEncode  xs))
				 |x=='w' = (".-- "++(morseEncode  xs))
				 |x=='x' = ("-..- "++(morseEncode  xs))
				 |x=='y' = ("-.-- "++(morseEncode  xs))
				 |x=='z' = ("--.. "++(morseEncode  xs))
				 |x=='0' = ("----- "++(morseEncode  xs))
				 |x=='1' = (".--- "++(morseEncode  xs))
				 |x=='2' = ("..--- "++(morseEncode  xs))
				 |x=='3' = ("...-- "++(morseEncode  xs))
				 |x=='4' = ("....- "++(morseEncode  xs))
				 |x=='5' = ("..... "++(morseEncode  xs))
				 |x=='6' = ("-.... "++(morseEncode  xs))
				 |x=='7' = ("--... "++(morseEncode  xs))
				 |x=='8' = ("---.. "++(morseEncode  xs))
				 |x=='9' = ("----. "++(morseEncode  xs))
				 |x=='.' = (".-.-.- "++(morseEncode  xs))
				 |x==',' = ("--..-- "++(morseEncode  xs))
				 |x=='?' = ("..--.. "++(morseEncode  xs))
				 |x=='-' = ("-....- "++(morseEncode  xs))
				 |x=='=' = ("-...- "++(morseEncode  xs))
				 |x==':' = ("---... "++(morseEncode  xs))
				 |x==';' = ("-.-.-. "++(morseEncode  xs))
				 |x=='(' = ("-.--. "++(morseEncode  xs))
				 |x==')' = ("-.--.- "++(morseEncode  xs))
				 |x=='/' = ("-..-. "++(morseEncode  xs))
				 |x=='"' = (".-..-. "++(morseEncode  xs))
				 |x=='$' = ("...-..- "++(morseEncode  xs))
				 |x=='\'' = (".----. "++(morseEncode  xs))
				 |x=='_' = ("..--.- "++(morseEncode  xs))
				 |x=='@' = (".--.-. "++(morseEncode  xs))
				 |x=='!' = ("..--. "++(morseEncode  xs))
				 |x=='+' = (".-.-. "++(morseEncode  xs))
				 |x=='~' = (".-... "++(morseEncode  xs))
				 |x=='#' = ("...-.- "++(morseEncode  xs))
				 |x=='&' = (".-... "++(morseEncode  xs))
				 |x==' ' = ("/ "++(morseEncode  xs))


{-
main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	let cipher = morseEncode samecasewords
	print cipher
-}