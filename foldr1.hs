add a b 		= a + b

foldr11 f z [] 		= z
foldr11 f z (x:xs)	= f x (foldr11 f z xs)
