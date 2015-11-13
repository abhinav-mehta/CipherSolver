fib2 = 1 : 1 : rest
	where rest = zipWith (+) fib2 (tail fib)
