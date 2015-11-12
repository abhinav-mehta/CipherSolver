import System.IO
import Data.List
import Data.Char (ord, chr)

atBashEncode :: Int -> String -> String
atBashEncode t = map f
 	where f c
      		| ord 'a' <= ord c && ord 'z' >= ord c = chr $ (t + ord c - ord 'a') `rem` 26 + ord 'a'
      		| ord 'A' <= ord c && ord 'Z' >= ord c = chr $ (t + ord c - ord 'A') `rem` 26 + ord 'A'
      		| True = c

main :: IO()

main = do print $ atBashEncode "PROGRAMMING PRAXIS"
