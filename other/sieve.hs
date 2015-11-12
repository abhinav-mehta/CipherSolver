primes = sieve [2..]
	where 	sieve (x:xs) 	= x : sieve ( filter (not . multipleOf x) xs )
		sieve _  		= []
		multipleOf x y 		= (y `mod` x == 0)
