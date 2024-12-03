module Parser (
runParser,
runParser',
char,
string,
eof,
oneOf,
integer,
newline,
whitespace,
Parser
) where

import Control.Applicative (Alternative (..))
import Control.Monad (liftM, ap)
import Data.List (nub, sort, elemIndex)
import Data.Either (fromRight)


newtype Parser a = Parser
  { runParser :: String ->  Either [String] (a, String) }

runParser' :: Parser a -> String -> a
runParser' p s = (fst . fromRight (error "oops")) $ runParser p s

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  (<*>) = ap

instance Monad Parser where
  return = pure
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Left [""]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left ["End of Input"]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left ["Unxpected " ++ [hd]]

whitespace :: Parser String
whitespace = some $ oneOf [' ', '\t']

lexeme :: Parser a -> Parser a
lexeme p = p <* many (oneOf [' ', '\t'])

char :: Char -> Parser Char
char i = satisfy (== i)

string :: String -> Parser String
string = lexeme . traverse char

eof :: Parser ()
eof = Parser $ \input ->
  case input of
    [] -> Right ((), [])
    (x:_)  -> Left ["Expected end of file, got " ++ [x]]

oneOf :: [Char] -> Parser Char
oneOf = foldl1 (<|>) . map char

numberChar :: Parser Char
numberChar = oneOf ['0'..'9']

integer :: Parser Int
integer = read <$> lexeme (some numberChar)

newline :: Parser Char
newline = char '\n'

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r p = l *> p <* r