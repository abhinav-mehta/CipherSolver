length1 [] 	= 0
length1 (_:xs)	= 1 + length1 xs
