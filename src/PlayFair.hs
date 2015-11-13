module PlayFair where

import Data.Char
import Data.List
import Data.List.HT
import Data.List.Split
import qualified Data.Map as M
 
type Key = M.Map (Int, Int) Char
 
process :: String -> String
process = replace "J" "I" . map toUpper . filter isLetter
 
key :: String -> Key
key = M.fromList . concat .
      zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0..]) [0..] .
      chunk 5 . (`union` delete 'J' ['A'..'Z']) . nub . process
 
bigram :: Key -> Int -> Char -> Char -> String
bigram k dir c1 c2
    | y1 == y2  = get (x1 + dir, y1) : [get (x2 + dir, y2)]
    | x1 == x2  = get (x1, y1 + dir) : [get (x2, y2 + dir)]
    | otherwise = get (x2, y1)       : [get (x1, y2)]
    where (x1, y1) = head . M.keys $ M.filter (== c1) k
          (x2, y2) = head . M.keys $ M.filter (== c2) k
          get (x,y) = k M.! (mod x 5, mod y 5)
 
encode' :: Key -> String -> String
encode' _ []       = []
encode' k [x]      = encode' k (x : "X")
encode' k (x:y:xs) | x == y    = encode' k [x] ++ encode' k (y:xs)
                   | otherwise = bigram k 1 x y ++ encode' k xs
 
decode' :: Key -> String -> String
decode' k = concatMap (\[x,y] -> bigram k (-1) x y) . chunk 2
 
playFairEncode :: String -> String -> String
encode k = encode' (key k) . process
 
playFairDecode :: String -> String -> String
decode k = decode' (key k) . process
 
{-
main :: IO ()
main = do print $ encode "PLAYFAIR" "PROGRAMMING PRAXIS"
          print $ decode "PLAYFAIR" "LIVOBLKZEDOELIYWCN"
-}