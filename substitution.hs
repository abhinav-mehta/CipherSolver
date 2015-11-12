import Data.List
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.Function

checkchar::[Char]->Char
checkchar (x:xs) = x

clearnoise :: [Char] -> [Char] -- as words function separates with space, there might be anomalies.
clearnoise [] = []
clearnoise (x:xs) | x=='.'	=clearnoise xs
				   | x==','	=clearnoise xs
				   | x=='!'	=clearnoise xs
				   | x=='?'	=clearnoise xs
				   | x==':'	=clearnoise xs
				   | x==';'	=clearnoise xs
				   | x=='"'	=clearnoise xs
				   | x=='{'	=clearnoise xs
				   | x=='}'	=clearnoise xs
				   | x=='('	=clearnoise xs
				   | x==')'	=clearnoise xs
				   | x==' ' = clearnoise xs
				   | otherwise = (x:(clearnoise xs))

reduceredundancy:: [String]->String
reduceredundancy [] = []
reduceredundancy (x:xs) = (checkchar x): (reduceredundancy xs)

lowerCase::String->String -- this makes the whole string in lower case so as to make "Hello" and "hello" same
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

maketuple::[Int]->[Char]->[(Int,Char)]
maketuple [] [] = []
maketuple (x:xs) (y:ys) = ((x,y)):(maketuple xs ys)

correcting::[String]->[String] -- this function calls clearnoise on every word
correcting [] = []
correcting (x:xs) = (clearnoise x):(correcting xs)

beechka:: String->[String]
beechka x = group (sort x)

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

letterfrequency :: [String] -> [Int]
letterfrequency x = fmap length (x)

main::IO() -- main function
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let samecasewords = lowerCase contents
	let wextract = words samecasewords
	let wfinal = correcting wextract
	let sortedwords = groupBy ((==) `on` length) $ sortBy (compare `on` length) wfinal
	let onlyalphabets = clearnoise samecasewords
	let digrams = groupBy ((==)) $ sort (merge (chunksOf 2 onlyalphabets) (chunksOf 2 (drop 1 onlyalphabets)))
	let groupedsorted = beechka onlyalphabets
	let frequencylist = letterfrequency groupedsorted
	let alphabetlist = reduceredundancy groupedsorted
	let tuples = maketuple frequencylist alphabetlist
	let sortedtuples = reverse (sort tuples)
	print digrams