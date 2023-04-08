-- correspond to closure-convert.sml

module ClConvert where

import qualified UnambiguousVScheme as X
import qualified ClScheme as C
import qualified Data.Set as S
import qualified ObjectCode as O

-- nr's set signature

--   type 'a set
--   val empty : 'a set
--   val member : ''a * ''a set -> bool
--   val insert : ''a * ''a set -> ''a set
--   val diff : ''a set * ''a set -> ''a set
--   val elems : 'a set -> 'a list
--   val ofList : ''a list -> ''a set

--   val union' : ''a set list -> ''a set  (* union of a list of sets *)

close :: X.Def -> C.Def
close = undefined

-- need to import O, cannot inherit the value constructor like the datatype
-- keyword in sml (* datatype literal = datatype ObjectCode.literal *)
literal lit = case lit of
    (X.Sym x) -> C.Literal (O.String x)
    (X.Int x) -> C.Literal (O.Int x)
    (X.Real x) -> C.Literal (O.Real x)
    (X.Bool x) -> C.Literal (O.Bool x)
    (X.EmptyList) -> C.Literal (O.EmptyList)


