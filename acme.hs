import Data.List
import Data.Maybe
import Data.Char
import System.Random
import Control.Monad.ST
import Data.STRef
import Control.Monad

import System.Random.Shuffle

-- These only work with ALLCAPSNOSPACESNOPUNCTUATION.

data Cipher key = Cipher (key -> String -> String) (key -> String -> String)

encode ~(Cipher f _) = f

decode ~(Cipher _ f) = f

-- | The Playfair cipher
playfair = Cipher start start where
	start k = fn (take 25 (nub (map (\ch -> if ch == 'J' then 'I' else ch) k)) ++ (['A'..'Z'] \\ ('J' : k)))
	fn k (c1 : c2 : s) = (k !! (5 * n + y)) : (k !! (5 * x + m)) : fn k s
		where
			(n, m) = fromJust (findIndex (==c1) k) `divMod` 5
			(x, y) = fromJust (findIndex (==c2) k) `divMod` 5
	fn _ s = s

swap (x, y) = (y, x)

-- | The letter substitution cipher
substitution = Cipher (\k s -> map (\ch -> fromJust $ lookup ch k) s) (\k s -> map (\ch -> fromJust $ lookup ch (map swap k)) s)

letter x = ord x - ord 'A' + 1

letter2 x = chr (x + ord 'A' - 1)

addLetters x y = if letter2 (letter x + letter y) > 'Z' then letter2 $ letter x + letter y - 26 else letter2 $ letter x + letter y

subtractLetters x y = if letter2 (letter x - letter y) < 'A' then letter2 $ letter x - letter y + 26 else letter2 $ letter x - letter y

-- | The Viginere cipher (acts like one-time pad if the key is as long as the plaintext)
viginere = Cipher (\k s -> zipWith addLetters s (cycle k)) (\k s -> zipWith subtractLetters s (cycle k))

-- | The Caesar cipher
caesar = Cipher (\k s -> map (`addLetters` k) s) (\k s -> map (`subtractLetters` k) s)

-- | For good measure, the German Enigma
data Rotor s = Rotor { mapping :: [(Char, Char)], position :: STRef s Char, nextRotor :: Maybe (Rotor s) }

advance rotor = do
	pos <- readSTRef (position rotor)
	let plus1 = addLetters pos 'A'
	writeSTRef (position rotor) plus1
	when (plus1 == 'A') $ maybe (return ()) advance (nextRotor rotor)

newRotor (RotorMapping mapping) init next = do
	pos <- newSTRef init
	return (Rotor mapping pos next)

newRotors (map1,map2,map3) (init1,init2,init3) = do
	rot1 <- newRotor map1 init1 Nothing
	rot2 <- newRotor map2 init2 (Just rot1)
	rot3 <- newRotor map3 init3 (Just rot2)
	return (rot1, rot2, rot3)

look ls x = fromJust (lookup x ls)

rotorFunction rotor = do
	pos <- readSTRef (position rotor)
	return (look (mapping rotor) . (`subtractLetters` pos))

reverseRotorFunction rotor = do
	pos <- readSTRef (position rotor)
	return ((`addLetters` pos) . look (map swap (mapping rotor)))

randomLetter stdgen = randomR ('A','Z') stdgen

newtype Plugboard = Plugboard [(Char, Char)]

newtype Reflector = Reflector [(Char, Char)]

newtype RotorMapping = RotorMapping [(Char, Char)]

encodeEnigma (Plugboard plugboard, Reflector reflector0, mappings) (init1,init2,init3) text = do
	let reflector = reflector0 ++ map swap reflector0
	-- Create the rotors
	(rotor1,rotor2,rotor3) <- newRotors mappings (init1,init2,init3)
	-- Translate the plaintext
	mapM (\letter -> do
			f1 <- rotorFunction rotor1
			f2 <- rotorFunction rotor2
			f3 <- rotorFunction rotor3
			f4 <- reverseRotorFunction rotor1
			f5 <- reverseRotorFunction rotor2
			f6 <- reverseRotorFunction rotor3
			advance rotor3
			return $ look (map swap plugboard) $ f6 $ f5 $ f4 $ look reflector $ f1 $ f2 $ f3 $ look plugboard letter)
		text

enigma stdgen = Cipher (\key plaintext -> runST $ do
	-- Choose an initial rotor position
	(init1, stdgen) <- return (randomLetter stdgen)
	(init2, stdgen) <- return (randomLetter stdgen)
	(init3, _) <- return (randomLetter stdgen)
	-- Do the cipher
	cipher <- encodeEnigma key (init1,init2,init3) plaintext
	-- Add the initial rotor position
	return (init1 : init2 : init3 : cipher))
	(\key (init1 : init2 : init3 : cipher) -> runST $ encodeEnigma key (init1,init2,init3) cipher)

rot13 = map (\x -> (x, addLetters x 'M')) ['A'..'N']

main = do
	ls <- shuffleM ['A'..'Z']
	let k = zip ['A'..] ls
	let key = (Plugboard k, Reflector rot13, (RotorMapping k, RotorMapping k, RotorMapping k))
	stdgen <- getStdGen
	putStrLn $ decode (enigma stdgen) key (encode (enigma stdgen) key "TESTINGTESTING")
	let k_num = 5
	putStrLn $ decode caesar k_num (encode caesar k_num "HELLO WORLD"
	putStrLn "Hello"

