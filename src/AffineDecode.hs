import System.IO
import Data.List
import Data.Char (ord, chr)

affineDecode :: Int -> Int -> String -> String
affineDecode a b = map f
 	where f c = chr $ (26 - a `mod` 26)*(ord c - ord 'A' - b) `mod` 26 + ord 'A'
