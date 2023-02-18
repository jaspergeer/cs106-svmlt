-- should be mimicing vscheme-parse.sml
-- Norman also separated the parsing of S-exp (sx),
-- definitions, and check/Expect shoulbe be put at the end

-- see parsing scheme file in uft.sml:
--   val schemeOfFile : instream -> VScheme.def list error =
--     lines                                   (* line list *)
--     >>>  SxParse.parse                      (* sx list error *)
--     >=>  Error.mapList VSchemeParsers.defs  (* def list list error *)
--     >>>  Error.map List.concat              (* def list error *)
--     >>>  Error.map VSchemeTests.delay

module VSchemeParse where

import qualified VScheme as S
import Text.Parsec.String ( Parser )
import Text.Parsec.Token ( symbol )
import Text.Parsec ( between
                   , char )

-- can't unerstand def of bracket in vscheme-parse.sml
-- bracket p = between (char '(') (char ')')

letstar :: [(S.Name, S.Exp)] -> S.Exp -> S.Exp
letstar [] e = e
letstar ((x, e') : bs) e = S.LetX S.Let [(x, e')] (letstar bs e)