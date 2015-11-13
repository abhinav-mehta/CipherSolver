map1 f [] 	= []
map1 f (x:xs) 	= f x : map1 f xs
