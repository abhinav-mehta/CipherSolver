foldr2 _ b [] 		= b
foldr2 o b (x:xs) 	= x `o` foldr2 o b xs
