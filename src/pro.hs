import qualified Data.ByteString.Char8 as B
--import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Control.Applicative((<$>), (<*))
import Network.HTTP
import Network.URI
import System.Environment
import Text.Parsec
import Data.List (nub)
import Data.Char
import qualified Data.Text as T

type AttrList = [(T.Text , T.Text)]

data NodeType = Text T.Text
              | Element T.Text AttrList
              deriving (Show)

data NTree a = NTree a [NTree a]
             | NullTree
             deriving (Show)

type HTMLTree = NTree NodeType

parseHtml :: T.Text -> Either ParseError HTMLTree
parseHtml s = case parse baseParser "" (T.unwords (T.words s)) of
                Left err -> Left err
                Right nodes -> Right $
                 if length nodes == 1
                     then head nodes
                     else case filter filterHelper nodes of
                            [] -> toTree "html" [] nodes
                            x  -> head x




main = do
  let url = "http://www.google.com" 
  --doc <- get url
  --return doc 
  putStrLn url



baseParser = docType <|> parseNodes

filterHelper :: HTMLTree -> Bool
filterHelper (NTree (Element x _) _ ) | x == "html" = True
filterHelper _ = False



attribute :: Stream s m Char => ParsecT s u m (T.Text, T.Text)
attribute = do
  name <- tagName
  spaces
  _ <- char '='
  spaces
  open <- char '\"' <|> char '\''
  -- value <- many (noneOf [open])
  value <- manyTill (try (char ' ') <|> anyChar) (try $ char open)
  return (T.pack name, T.pack value)


toLeaf::T.Text -> HTMLTree
toLeaf t = NTree (Text t) []

toTree::T.Text -> AttrList -> [HTMLTree] -> HTMLTree
toTree t l = NTree (Element t l)

tagName :: Stream s m Char => ParsecT s u m String
tagName = many1 (try (char ':') <|> try (char '-') <|> try (char '_') <|> try (char '.') <|> try (char ';') <|> alphaNum)

parseNodes :: Stream s m Char => ParsecT s u m [HTMLTree]
parseNodes = manyTill parseNode last'
  where
    last' = eof <|> void (try (string "</"))

parseNode :: Stream s m Char => ParsecT s u m HTMLTree
parseNode =  try oneLiners <|> try docTypeHandler<|> parseElement <|> parseText

docType = do
  try docTypeHandler
  parseNodes

docTypeHandler = (toLeaf . T.pack) <$> do
  _ <- spaces
  _ <- string "<!"
  _ <- manyTill anyChar (char '>')
  return ""

oneLiners = (toLeaf . T.pack) <$> do
  _ <- spaces
  _ <- char '<'
  _ <- manyTill (noneOf ">") $try (string "/>")
  return ""

parseText :: Stream s m Char => ParsecT s u m HTMLTree
parseText =  (toLeaf . T.pack) <$>do
  _ <- spaces
  many (noneOf "<")

parseElement :: Stream s m Char => ParsecT s u m HTMLTree
parseElement = do
  (tag, attrs) <- between (char '<') (char '>') tagData
  tempTag tag attrs


tagData :: Stream s m Char => ParsecT s u m (String, [(T.Text, T.Text)])
tagData = do
  t <- tagName
  attrs <- attributes
  return (t,attrs)

attributes :: Stream s m Char => ParsecT s u m [(T.Text, T.Text)]
attributes =  spaces >> many (trailingSpaces (try attribute <|> try attribute' <|> try simpleTextInsideTag))

simpleTextInsideTag = do
  a <- many1 alphaNum
  spaces
  return (T.pack a, T.pack "")



attribute' :: Stream s m Char => ParsecT s u m (T.Text, T.Text)
attribute' = do
  name <- tagName
  spaces
  _ <- char '='
  spaces
  value <- many (noneOf "> ")
  return (T.pack name, T.pack value)


tempTag :: Stream s m Char => String -> [(T.Text, T.Text)] -> ParsecT s u m HTMLTree
tempTag tag attrs
  | map toLower tag `elem` exceptionList = return $ toTree (T.pack tag) attrs []
  | map toLower tag == "script" = do
    _ <- manyTill anyChar (try $string "</script")
    return $ toTree (T.pack tag) attrs []
  | map toLower tag == "style" = do
    _ <- manyTill anyChar (try $string "</style")
    return $ toTree (T.pack tag) attrs []
  | otherwise = do
    children <- parseNodes
    _ <- string $tag ++ ">"
    return $ toTree (T.pack tag) attrs $nub children


trailingSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
trailingSpaces = (<* spaces)


exceptionList :: [String]
exceptionList = ["link", "br", "img", "meta", "hr", "input"]


type Name = T.Text

type Class = Maybe T.Text

type ID = Maybe T.Text

data NodeQuery = NodeQuery Name Class ID deriving (Show, Read)

type Query = [NodeQuery]