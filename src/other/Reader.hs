module Reader where

import System.IO
import Data.Text
import Text.Parsec 
import Text.Parsec.String
import Control.Applicative ((<$>))

data Tree = Leaf String	
	| Node [Tree] 

main :: IO()
main = do
	let raw = hGetContents =<< openFile "test.html" ReadMode
	raw >>= putStrLn
	raw2 <- raw 
	parseTest parseGroups raw2

parseTree :: Parser Tree
parseTree = tex <|> node <|> leaf
	where 
		node = Node <$> between (char '>') (char '/') (many parseTree)
		leaf = Leaf <$> many1 (noneOf "<>")

nodes :: Tree -> [Tree]
nodes (Leaf x) = [Leaf x]
nodes t@(Node ts) = t : Prelude.concatMap nodes ts

instance Show Tree where
  showsPrec d (Leaf x) = showString x
  showsPrec d (Node xs) = showString "<" . showList xs . showString ">"
    where
      showList [] = id
      showList (x:xs) = shows x . showList xs

parseGroups :: Parser [String]
parseGroups = Prelude.map show . nodes <$> parseTree

