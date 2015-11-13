module WordCount where

import System.IO
import Data.List
import Data.Char
import Data.Functor

main :: IO()
main = do	
		fileName <- getLine
		inputHandle <- openFile fileName ReadMode
		raw <- fmap (map cleaner) (hGetContents inputHandle)

		putStrLn $ "WORD COUNTER: " ++ numberWords  raw
		putStrLn $ "DISTINCT WORDS: " ++ distinctWords raw
 		putStrLn $ "FREQUENCY: " ++ frequencyWords raw		

cleaner :: Char -> Char
cleaner x | isAlpha x = toLower x
	  | True      = ' '	


numberWords :: String -> String
numberWords = show . length . words


distinctWords :: String -> String
distinctWords = unwords . map head . group . sort . words


frequencyWords :: String -> String
frequencyWords = show . map (\x -> (head x, length x)) . group . sort . filter (`notElem` noise) . words


noise = ["a","the","and","be","to","of","in","that","have","it","for","not","on","with","he","as","you","do","at","this","but","his","by","from","they","we","say","her","she","or","an","will","my","one","all","would","there","their","what","1","2","3","4","5","6","7","8","9","0"]

