import Data.List
import Data.Maybe
import Data.Char
import System.Random
import Control.Monad.ST
import Data.STRef
import Control.Monad

import System.Random.Shuffle

shift x k = fmap (letter2 k) $ fmap letter x
letter x=ord x - ord 'A' + 0
letter2 k x = chr (x+ ord 'A' - k)

main=do
	let xx="UG XZWRMKB"
	let dict=["MY", "PROJECT", "HELLO", "IN"]
	putStrLn $ shift xx 8
	putStrLn "Hello"
