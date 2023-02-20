-- Embeds KNormal-form Scheme into VScheme. This cannot fail.
module KNEmbed where

import qualified KNF as K
import qualified VScheme as S
import qualified Primitives as P
import qualified ObjectCode as O

let' x e' e = S.LetX S.Let [(x, e')] e

value :: K.Literal -> S.Value
value (O.Int i) = S.Int i

