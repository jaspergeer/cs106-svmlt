module SxParse where

import qualified Sx
import qualified ParseUtils
import Text.Parsec ( spaces,
                     ParseError,
                     char,
                     try,
                     (<|>),
                     alphaNum,
                     many1,
                     many, satisfy )
import Text.Parsec.String ( Parser )
import Data.Char (isSpace, isDigit)

bool = ParseUtils.bool
lexeme = ParseUtils.lexeme
int = ParseUtils.int
double = ParseUtils.double
tok = ParseUtils.token
reserved = "()[]#'\"`"
name :: Parser String
name = ParseUtils.lexeme $ (:) <$> satisfy (\x -> not (isSpace x || elem x reserved || isDigit x))
  <*> many (satisfy (\x -> not (isSpace x || elem x reserved)))

sx :: Parser Sx.Sx
sx = char '\'' *> (Sx.List <$> (tok "(" *> many sx' <* tok ")")
              <|> Sx.Sym <$> name)
    <|> nums where
      nums = try (Sx.Int <$> int)
        <|> Sx.Real <$> double
        <|> Sx.Bool <$> bool
      sx' = Sx.List <$> (tok "(" *> many sx' <* tok ")")
        <|> Sx.Sym <$> name
        <|> nums