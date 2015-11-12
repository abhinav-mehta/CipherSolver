punctuate [] 	= []
punctuate [x]	= [x]
punctuate [x,y]	= [x,"and",y]
punctuate (x:xs)= comma x : punctuate xs

comma  = (++ ",")
