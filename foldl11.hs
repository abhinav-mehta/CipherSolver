add a b 		= a + b

foldl11 _ z []		= z
foldl11 f z (x:xs)	= foldl11 f (f z x) xs

