module SxParse where

import qualified Sx
import Text.Parsec (spaces)
import Text.Parsec.String ( Parser )

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

