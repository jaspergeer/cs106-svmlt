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
import qualified ParseUtils
import qualified Sx
import qualified SxParse
import Text.Parsec.String ( Parser )
import Text.Parsec.Token ( symbol )
import Text.Parsec ( between,
                     char,
                     (<|>),
                     try, many )

valOfSx s = case s of
  Sx.Int i -> S.Int i
  Sx.Bool b -> S.Bool b
  Sx.Sym x -> S.Sym x
  Sx.Real n -> S.Real n
  Sx.List (x:xs) -> S.Pair (valOfSx x) (valOfSx (Sx.List xs))
  Sx.List [] -> S.EmptyList

tok = ParseUtils.token
int = ParseUtils.int
double = ParseUtils.double
bool = ParseUtils.bool
name = ParseUtils.name

parend :: Parser a -> Parser a
parend p = tok "(" *> p <* tok ")"

brackd :: Parser a -> Parser a
brackd p = tok "[" *> p <* tok "]"

formals = many name

expr :: Parser S.Exp
expr = let
  letKind = S.Let <$ tok "let"
        <|> S.LetRec <$ tok "letrec"
  bind = brackd ((,) <$> name <*> expr)
  expr' = S.Set <$> (tok "set" *> name) <*> expr
      <|> S.IfX <$> (tok "if" *> expr) <*> expr <*> expr
      <|> S.WhileX <$> (tok "while" *> expr) <*> expr
      <|> S.Begin <$> (tok "begin" *> many expr)
      <|> S.Lambda <$> (try (tok "lambda") *> brackd formals) <*> expr
      <|> S.LetX <$> letKind <*> parend (many bind) <*> expr
      <|> S.Apply <$> expr <*> many expr
  in
    parend expr'
    <|> S.Var <$> name
    <|> S.Literal . valOfSx <$> SxParse.sx

-- dunno what this is for
-- letstar :: [(S.Name, S.Exp)] -> S.Exp -> S.Exp
-- letstar [] e = e
-- letstar ((x, e') : bs) e = S.LetX S.Let [(x, e')] (letstar bs e)

def :: Parser S.Def
def = let
  def' = S.Val <$> (tok "val" *> name) <*> expr
     <|> S.Define <$> (tok "define" *> name) <*> parend formals <*> expr
     <|> S.CheckExpect <$> (tok "check-expect" *> expr) <*> expr
     <|> S.CheckAssert <$> (tok "check-assert" *> expr)
  in parend def'
 <|> S.Exp <$> expr

defs :: Parser [S.Def]
defs = many def -- ????