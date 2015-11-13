module RSAencode where

import System.IO
import Data.Numbers.Primes
import Data.List
import Data.Char(ord,chr)

modulus::Int->Int->Int->Int
modulus x 0 z = 1
modulus x y z = mod ((mod x z)*(modulus x (y-1) z)) z

msgmaker::[Char] -> [Int]
msgmaker [] = []
msgmaker [x] = ((ord x)*1000):[]
msgmaker (x:y:xs) = (((ord x)*1000)+(ord y)):(msgmaker xs)

ciphermaker::[Int]->Int->Int->[Int]
ciphermaker [] _ _ = []
ciphermaker (x:xs) y z = (modulus x y z):(ciphermaker xs y z)

textmaker::[Int] -> [Char]
textmaker [] = []
textmaker (x:xs) = (chr ((x `div` 1000)::Int)):(chr (mod x 1000)):(textmaker xs)

{-
main::IO()
main = do
	inh <- openFile "input.txt" ReadMode
	contents <- hGetContents inh
	let message = msgmaker contents
	keyfile <- openFile "keyrsa.txt" ReadMode
	keys <- hGetContents keyfile
	let keystr = words keys
	let key = (read (keystr!!0)::Int):(read (keystr!!1)::Int):[]
	let cipher = ciphermaker message (key!!0) (key!!1)
	print cipher
-}
