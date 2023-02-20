module SxParse where

import qualified Sx
import qualified ParseUtils
import Text.Parsec ( spaces,
                     ParseError,
                     char )
import Text.Parsec.String ( Parser )

bool = ParseUtils.bool
name = ParseUtils.name
lexeme = ParseUtils.lexeme

parse :: Parser [Sx.Sx]
parse = error "implementme"

quote = lexeme (char '"')

