module ParseUtils where

import Text.Parsec
import Text.Parsec.String
import qualified Error as E

parseAndErr :: Parser a -> String -> E.Error a
parseAndErr p input = case runParser p () "" input of
  Left e -> E.Error $ Left (show e)
  Right r -> E.Error $ Right r

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

int :: Parser Int
int = lexeme $ read <$> ((++) <$> option "" (string "-") <*> many1 digit)

bool :: Parser Bool
bool = lexeme $ char '#' *> (True <$ char 't' <|> False <$ char 'f')

token :: String -> Parser String
token w = lexeme $ string w

double :: Parser Double
double = lexeme $ read <$> ((++) <$> whole <*> decimal)
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

-- same behavior as manyTill except first parser is tried before the second
manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p1 p2 = try ((:) <$> p1 <*> manyTill' p1 p2) <|> ([] <$ p2)