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
                     many )
import Text.Parsec.String ( Parser )

bool = ParseUtils.bool
name = ParseUtils.name
lexeme = ParseUtils.lexeme
int = ParseUtils.int
double = ParseUtils.double
tok = ParseUtils.token

sx :: Parser Sx.Sx
sx = Sx.List <$> (tok "'(" *> many sx' <* tok ")")
    <|> sx' where
      sx' = try (Sx.Int <$> int)
        <|> Sx.Real <$> double
        <|> Sx.Sym <$> (char '\'' *> many1 alphaNum)
        <|> Sx.Bool <$> bool
        <|> Sx.List <$> (tok "(" *> many sx' <* tok ")")