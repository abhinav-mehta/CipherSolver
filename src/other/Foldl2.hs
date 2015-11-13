foldl2 _ b [] 		= b
foldl2 o b (x:xs)	= foldl2 o (b `o` x) xs 
