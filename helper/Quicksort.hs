module Quicksort where
import Partition

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = 	quicksort lt ++ x : quicksort rt 
			where (lt , rt) = partitionTwo x xs
