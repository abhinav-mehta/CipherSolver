foldr1 f z [] 		= z
foldr1 f z (x:xs)	= f x (foldr1 f z xs)
