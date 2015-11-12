module CaesarDecode where

import CaesarEncode

caesarDecode :: Int -> String -> String
caesarDecode t = caesarEncode (-t)