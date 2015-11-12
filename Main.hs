module Main where

import System.IO
import Data.List

main :: IO()
main = do
	data1 <- hGetContents =<< openFile "american-english" ReadMode
	data2 <- hGetContents =<< openFile "cracklib-small" ReadMode
	let bigData = sort $ (words data1) ++ (words data2)
	print bigData
	