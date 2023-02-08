module AsmParse where

import qualified Asm as A
import qualified ObjectCode as O
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec.ByteString ( Parser )
import Text.Parsec
    ( digit,
      many1,
      (<|>),
      string,
      alphaNum,
      char,
      option,
      oneOf,
      spaces,
      many,
      manyTill,
      space,
      newline,
      choice,
      (<?>),
      try,
      anyChar )

-- parsing

name :: Parser String
name = (:) <$> oneOf (['a'..'z'] ++ ['A'..'Z']) <*> many alphaNum

line :: Parser a -> Parser a
line p = spaces *> p <* newline

integer :: Parser Integer
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

nat :: Parser Int
nat = read <$> many1 digit

double :: Parser Double
double = read <$> ((++) <$> whole <*> decimal)
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

literal :: Parser O.Literal
literal = try (O.String <$> (char '\"' *> manyTill anyChar (char '\"')))
        <|> try (O.Real <$> double)
        <|> try (O.Int <$> integer)
        <|> try (O.Bool True <$ string "#t")
        <|> try (O.Bool False <$ string "#f")
        <|> try (O.EmptyList <$ string "'()'")
        <|> try (O.Nil <$ string "nil")

regs :: O.Operator -> [O.Reg] -> A.Instr
regs op operands = A.ObjectCode (O.Regs op operands)

reg :: Parser Int
reg = read <$> (char 'r' *> many1 digit)

spc :: Parser a -> Parser a
spc p = space *> p

eR0 op = regs op []
eR1 op r1 = regs op [r1]
eR2 op r1 r2 = regs op [r1, r2]
eR3 op r1 r2 r3 = regs op [r1, r2, r3]
eR1LIT op r1 lit = A.ObjectCode (O.RegLit op r1 lit)
eR1GLO op r1 name = A.ObjectCode (O.RegGlo op r1 name)

type Short = String

oneOfStr :: [String] -> Parser String
oneOfStr strs = choice (map string strs)

singleLineInstr :: Parser A.Instr
singleLineInstr = line
  -- standard cases
  (
  try binop
  <|> try (regInstr eR3 A.opcodesR3 <*> spc reg <*> spc reg)
  <|> try (regInstr eR2 A.opcodesR2 <*> spc reg)
  <|> try (regInstr eR1LIT A.opcodesR1LIT <*> spc literal)
  <|> try (regInstr eR1 A.opcodesR1)
  <|> try (eR0 <$> oneOfStr A.opcodesR0)
  -- syntactic sugar
  <|> try (eR1 "zero" <$> reg <* spc (string ":=") <* spc (char '0'))
  <|> try (eR1LIT "loadliteral" <$> reg <* spc (string ":=") <*> spc literal)
  <|> try (eR1GLO "loadglobal" <$> reg <* spc (string ":=")
        <* spc (string "G[") <*> name <* char ']')
  <|> try setGlobal
  )
  where regInstr eRX opcodes = do
          r1 <- reg
          spc (string ":=")
          op <- spc (oneOfStr opcodes)
          return (eRX op r1)
        binop = do
          r1 <- reg
          spc (string ":=")
          r2 <- spc reg
          op <- spc (oneOfStr A.binops)
          r3 <- spc reg
          return $ eR3 op r1 r2 r3
        setGlobal = do
          string "G["
          n <- name
          char ']'
          spc (string ":=")
          r1 <- spc reg
          return (eR1GLO "setglobal" r1 n)

loadFunc arity body x = A.LoadFunc x arity body

instruction :: Parser A.Instr
instruction = (try singleLineInstr
            <|> try (loadFunBegin <*> manyTill instruction loadFunEnd))
            <?> "an assembly instruction"
            where loadFunBegin = line $ do
                    string ".loadfunc"
                    arity <- spc nat
                    return (\body -> loadFunc arity body (length body))
                  loadFunEnd = line $ string ".loadend"

asmParse :: Parser [A.Instr]
asmParse = many instruction