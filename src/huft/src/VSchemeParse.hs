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
import qualified VSchemeUtils as U
import qualified ParseUtils
import qualified Sx
import qualified SxParse
import Text.Parsec.String ( Parser )
import Text.Parsec.Token ( symbol )
import Text.Parsec ( between,
                     char,
                     (<|>),
                     try, many, manyTill, anyChar, spaces, endOfLine, sepBy, skipMany, eof, satisfy )
import Data.Char (isSpace, isDigit)


valOfSx s = case s of
  Sx.Int i -> S.Int i
  Sx.Bool b -> S.Bool b
  Sx.Sym x -> S.Sym x
  Sx.Real n -> S.Real n
  Sx.List (x:xs) -> S.Pair (valOfSx x) (valOfSx (Sx.List xs))
  Sx.List [] -> S.EmptyList

deComment xs = let
  inComment xs = case xs of
    ('\n':xs) -> '\n' : outComment xs
    (x:xs) -> ' ' : inComment xs
    [] -> []
  outComment xs = case xs of
    ('\\':';':xs) -> '\\' : ';' : outComment xs
    (';':xs) -> ' ' : inComment xs
    (x:xs) -> x : outComment xs
    [] -> []
  in outComment xs

tok = ParseUtils.token
int = ParseUtils.int
double = ParseUtils.double
bool = ParseUtils.bool
name = SxParse.name

parend :: Parser a -> Parser a
parend p = tok "(" *> p <* tok ")"

brackd :: Parser a -> Parser a
brackd p = tok "[" *> p <* tok "]"

formals = many name
bind = brackd ((,) <$> name <*> expr)
expr :: Parser S.Exp
expr = let
  letstar [] e = e
  letstar ((x, e') : bs) e = S.LetX S.Let [(x, e')] (letstar bs e)
  letKind = try (S.LetRec <$ tok "letrec")
        <|> S.Let <$ try (tok "let")
  
  expr' = S.Set <$> try (tok "set " *> name) <*> expr
      <|> S.IfX <$> try (tok "if" *> expr) <*> expr <*> expr
      <|> S.WhileX <$> try (tok "while" *> expr) <*> expr
      <|> S.Begin <$> try (tok "begin" *> many expr)
      <|> S.Lambda <$> try (try (tok "lambda") *> parend formals) <*> expr
      <|> letstar <$> try (try (tok "let*") *> parend (many bind)) <*> expr
      <|> S.LetX <$> letKind <*> parend (many bind) <*> expr
      <|> S.Apply <$> expr <*> many expr
  in
    parend expr'
    <|> try (S.Literal . valOfSx <$> SxParse.sx)
    <|> S.Var <$> name

-- record desugaring

nullp x = S.Apply (S.Var "null?") [x]
pairp x = S.Apply (S.Var "pair?") [x]
cons = U.cons

desugarRecord recname fieldnames =
              recordConstructor recname fieldnames :
              recordPredicate recname fieldnames :
              recordAccessors recname 0 fieldnames ++
              recordMutators recname 0 fieldnames
  where recordConstructor recname fieldnames =
          let con = "make-" ++ recname
              formals = map ("the-" ++) fieldnames
              body = cons (S.Literal (S.Sym con)) (varlist formals)
          in S.Define con formals body
        recordPredicate recname fieldnames = 
          let tag = S.Sym ("make-" ++ recname)
              predname = recname ++ "?"
              r = S.Var "r"
              formals = ["r"]
              goodCar = S.Apply (S.Var "=") [U.car r, S.Literal tag]
              goodCdr lookingAt [] = nullp lookingAt
              goodCdr lookingAt (_:rest) =
                andAlso (pairp lookingAt) (goodCdr (U.cdr lookingAt) rest)
              body = andAlso (pairp r) (andAlso goodCar (goodCdr (U.cdr r) fieldnames))
          in S.Define predname formals body
        recordAccessors recname n [] = []
        recordAccessors recname n (field:fields) =
          let predname = recname ++ "?"
              accname = recname ++ "-" ++ field
              formals = ["r"]
              thefield = U.car (cdrs (n + 1) (S.Var "r"))
              body = S.IfX (S.Apply (S.Var predname) [S.Var "r"])
                            thefield
                            (error (S.Sym (concat
                            ["value-passed-to-"
                            , accname
                            , "-is-not-a-"
                            , recname
                            ])))
          in S.Define accname formals body : recordAccessors recname (n + 1) fields
        recordMutators recname n [] = []
        recordMutators recname n (field:fields) =
          let predname = recname ++ "?"
              mutname = "set-" ++ recname ++ "-" ++ field ++ "!"
              formals = ["r", "v"]
              setfield = U.setcar (cdrs (n + 1) (S.Var "r")) (S.Var "v")
              body = S.IfX (S.Apply (S.Var predname) [S.Var "r"])
                            setfield
                            (error (S.Sym (concat
                            ["value-passed-to"
                            , mutname
                            , "-is-not-a-"
                            , recname
                            ])))
          in S.Define mutname formals body : recordMutators recname (n + 1) fields

        andAlso p q = S.IfX p q (S.Literal (S.Bool False))

        cdrs 0 xs = xs
        cdrs n xs = U.cdr (cdrs (n - 1) xs)

        list [] = S.Literal S.EmptyList
        list (v:vs) = cons (S.Literal v) (list vs)
        error x = S.Apply (S.Var "error") [S.Literal x]

        varlist [] = S.Literal S.EmptyList
        varlist (x:xs) = cons (S.Var x) (varlist xs)

single x = [x]

def :: Parser [S.Def]
def = let
  def' = S.Val <$> try (tok "val " *> name) <*> expr
     <|> S.Define <$> try (tok "define " *> name) <*> parend formals <*> expr
     <|> S.CheckExpect <$> try (tok "check-expect" *> expr) <*> expr
     <|> S.CheckAssert <$> try (tok "check-assert" *> expr)
     <|> S.Use <$> try  (tok "use " *> name)
  in try (parend (single <$> def'
 <|> desugarRecord <$> (tok "record" *> name) <*> brackd (many name)))
 <|> single . S.Exp <$> expr

-- comment :: Parser ()
-- comment = () <$ tok ";" <* manyTill anyChar endOfLine <* spaces

parse :: Parser [S.Def]
parse = spaces *> (concat <$> manyTill def eof)

-- defs :: Parser [S.Def]
-- defs = many def -- ????