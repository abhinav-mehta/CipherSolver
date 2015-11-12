import Data.Numbers.Primes
import Data.List
import System.IO
import System.Random
import Control.Monad (replicateM)

choosewisely::Int->[Int]
choosewisely x 
	| length (primeFactors (x+1)) == 2 = (primeFactors (x+1))
	| otherwise = choosewisely (2*x)

modulus:: Int -> [Int] -> [Int]
modulus y [] = []
modulus y (x:xs) = (mod x y:(modulus y xs))


main::IO()
main = do
	x <- replicateM 3 (randomIO :: IO Int)
	let nth = sort (modulus 500 x)
	let nprimes = (take (nth !! 2) primes)
	let pq = (nprimes !! (nth !! 0)):(nprimes !! (nth !! 1)):[]
	let n = (pq !! 0)*(pq !! 1)
	let phin = ((pq !! 0)-1)*((pq !! 1)-1)
	let ed = choosewisely phin
	print ed
	print n
	print phin