foldl1 _ z []		= z
foldl1 f z (x:xs)	= foldl1 f (f z x) xs

