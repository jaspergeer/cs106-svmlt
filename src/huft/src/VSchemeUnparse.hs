module VSchemeUnparse where

import qualified VScheme as S
import           Prettyprinter                (Pretty (pretty), vsep)

letkeyword S.Let = P.pretty "let"
letkeyword S.LetStar = P.pretty "let*"
