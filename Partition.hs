module Partition(partitionTwo) where

partitionTwo :: Ord a => a -> [a] -> ([a],[a])
partitionTwo _ [] = ([],[])
partitionTwo a (x:xs) 	| x <= a = (x:lt,rt)
			| x > a  = (lt,x:rt)
			where (lt,rt) = partitionTwo a xs
