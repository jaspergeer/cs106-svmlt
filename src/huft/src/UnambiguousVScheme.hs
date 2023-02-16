module UnambiguousVScheme where

import qualified Primitives as P
import qualified VScheme as VS

type Name = String

data Exp = Literal VS.Value
         | Local Name