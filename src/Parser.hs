{-# LANGUAGE OverloadedStrings #-}

module Parser
    (
    ) where


import           Control.Applicative
import           Core
import           Data.Attoparsec.Text


oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepBy p sep <* optional sep

endBy :: (Monad f, Alternative f) => f a -> f b -> f [a]
endBy p sep = many $ p <* sep

spaces :: Parser ()
spaces = skipMany space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser Sexpr
parseString = String <$> (char '"' *> many (notChar '"') <* char '"')

parseNumber :: Parser Sexpr
parseNumber = Number . read <$> many1 digit

parseAtom :: Parser Sexpr
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseQuoted :: Parser Sexpr
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseList :: Parser Sexpr
parseList = List <$> sepEndBy parseExpr spaces

parseDottedList :: Parser Sexpr
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseExpr :: Parser Sexpr
parseExpr =
      parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
    _ <- spaces
    _ <- char '('
    _ <- spaces
    x <- try parseList <|> parseDottedList
    _ <- spaces
    _ <- char ')'
    _ <- spaces
    return x



parser :: Parser [Sexpr]
parser = many1 parseExpr
