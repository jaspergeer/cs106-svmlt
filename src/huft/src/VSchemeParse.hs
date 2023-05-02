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
import qualified Pattern as P
import qualified Case

import Text.Parsec.String ( Parser )
import Text.Parsec.Token ( symbol )
import Text.Parsec ( between,
                     char,
                     (<|>),
                     try,
                     many,
                     manyTill,
                     anyChar,
                     spaces,
                     endOfLine,
                     sepBy,
                     skipMany,
                     eof,
                     satisfy,
                     digit,
                     many1 )
import Data.Char (isSpace, isDigit)

import Debug.Trace

rwords =
  [ "set", "if", "while", "begin", "let", "let*", "letrec", "lambda",
    "quote","val", "define", "case", "data", "implicit-data",
    "check-principal-type*", "record", "check-type", ":" ]

reserved x = x `elem` rwords

isVcon x = case x of
  (y:ys) | y `elem` ['A'..'Z'] -> True
  ('m':'a':'k':'e':'-':xs) -> True
  ('#':xs) -> True
  "cons" -> True
  "'()" -> True
  _ -> False

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

brackd :: Parser a -> Parser a
brackd p = tok "[" *> p <* tok "]"
        <|> tok "(" *> p <* tok ")"

sat :: (Show a) => (a -> Bool) -> Parser a -> Parser a
sat p parser = do
  v <- parser
  if p v then return v else fail ("not expecting " ++ show v)

vcon =
  let
    isEmptyList S.EmptyList = True
    isEmptyList _ = False
    boolname p = if p then "#t" else "#f"
  in boolname <$> bool
    <|> sat isVcon name
    <|> "'()" <$ brackd (tok "quote" *> sat (isEmptyList . valOfSx) SxParse.sx) -- definately wrong

pattern :: Parser P.Pat
pattern = try (P.Apply <$> vcon <*> return [])
       <|> try (P.Apply <$> (show <$> int) <*> return [])
       <|> try (P.Wildcard <$ tok "_")
       <|> try (P.Var <$> name)
       <|> try (P.Apply <$> tok "'()" <*> return []) -- short fix for me
      --  <|> try (brackd pattern)
       <|> try (brackd (P.Apply <$> vcon <*> many pattern))

formals :: Parser [String]
formals = many name

bind :: Parser (String, S.Exp)
bind = brackd ((,) <$> name <*> expr)

fresh :: S.Exp -> [Char]
fresh e =
  let attempt n = let 
                    x = "x" ++ show n
                  in 
                    if freeIn e x then attempt (n + 1) else x
    in attempt 1

orSugar :: [S.Exp] -> S.Exp
orSugar ez = case ez of
    [] -> S.Literal (S.Bool False)
    [e] -> e
    (e1:es) ->
        let e2 = orSugar es
            x = fresh e2
        in  S.LetX S.Let [(x, e1)] (S.IfX (S.Var x) (S.Var x) e2)

andSugar :: [S.Exp] -> S.Exp
andSugar ez = case ez of
  [] -> S.Literal (S.Bool True)
  [e] -> e
  (e:es) -> S.IfX e (andSugar es) (S.Literal (S.Bool False))

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
      <|> S.Lambda <$> try (try (tok "lambda") *> brackd formals) <*> expr
      <|> letstar <$> try (try (tok "let*") *> brackd (many bind)) <*> expr
      <|> let
        choice = brackd ((,) <$> pattern <*> expr)
        caset = Case.T <$> expr <*> many choice
        in S.Case <$> try (tok "case" *> caset)
      <|> S.LetX <$> letKind <*> brackd (many bind) <*> expr
      <|> tok "||" *> (orSugar <$> many expr)
      <|> tok "&&" *> (andSugar <$> many expr)
      <|> S.Apply <$> expr <*> many expr
  in
    brackd expr'
    <|> try (do
      v <- valOfSx <$> SxParse.sx
      case v of
        S.EmptyList -> return (S.VCon "'()")
        _ -> return (S.Literal v))
    -- swapping these two lines is the difference between es/ho
    <|> S.VCon <$> try vcon
    <|> S.Var <$> try name

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
              body = S.Apply (S.VCon con) (map S.Var formals)
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
     <|> S.Define <$> try (tok "define " *> name) <*> brackd formals <*> expr
     <|> S.CheckExpect <$> try (tok "check-expect" *> expr) <*> try expr
     <|> S.CheckAssert <$> try (tok "check-assert" *> expr)
     <|> S.Use <$> try  (tok "use " *> name)
  in try (brackd (single <$> def'
 <|> desugarRecord <$> (tok "record" *> name) <*> brackd (many name)))
 <|> single . S.Exp <$> expr

-- comment :: Parser ()
-- comment = () <$ tok ";" <* manyTill anyChar endOfLine <* spaces

parse :: Parser [S.Def]
parse = spaces *> (concat <$> manyTill def eof)

-- defs :: Parser [S.Def]
-- defs = many def -- ????

freeIn exp y =
  let member y [] = False
      member y (z:zs) = y == z || member y zs
      hasY e = case e of
        S.Literal _ -> False
        S.Var x -> x == y
        S.Set x e -> x == y || hasY e
        S.IfX e1 e2 e3 -> any hasY [e1, e2, e3]
        S.WhileX e1 e2 -> any hasY [e1, e2]
        S.Begin es -> any hasY es
        S.Apply e es -> any hasY (e:es)
        S.LetX S.Let bs e -> any rhsHasY bs || hasY e
        S.LetX S.LetRec bs e -> notElem y (map fst bs) && hasY e
        S.Lambda xs e -> notElem y xs && hasY e
        S.VCon _ -> False
        S.Case (Case.T e choices) -> hasY e || any choiceHasY choices
        S.Cond qas -> any (\(q, a) -> hasY q || hasY a) qas
      choiceHasY (pat, exp) = not (bindsY pat) && hasY exp
      bindsY pat = case pat of
        P.Var x -> x == y
        P.Wildcard -> False
        P.Apply _ pats -> any bindsY pats
      rhsHasY (_, e) = hasY e
  in hasY exp