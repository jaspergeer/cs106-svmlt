module AsmParse where

import qualified Asm as A
import qualified ObjectCode as O
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec.String ( Parser )
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
      optional,
      manyTill,
      space,
      newline,
      choice,
      (<?>),
      try,
      anyChar,
      eof,
      endOfLine, skipMany )
import Text.Parsec.Char (noneOf)

-- parsing

name :: Parser String
name = (:) <$> oneOf (['a'..'z'] ++ ['A'..'Z']) <*> many alphaNum

line :: Parser a -> Parser a
line p = spaces *> p <* newline

integer :: Parser Int
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

nat :: Parser Int
nat = read <$> many1 digit

double :: Parser Double
double = read <$> ((++) <$> whole <*> decimal)
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

-- same behavior as manyTill except first parser is tried before the second
manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p1 p2 = try ((:) <$> p1 <*> manyTill' p1 p2) <|> ([] <$ p2)

stringLit :: Parser String
stringLit = char '"' *> manyTill' (try escape <|> noneOf "\n") (char '"')
  where escape = do
          char '\\'
          c <- oneOf "abtnr\"\\"
          return $ case c of
            'a' -> '\a'
            'b' -> '\b'
            't' -> '\t'
            'n' -> '\n'
            'r' -> '\r'
            '\"' -> '\"'
            '\\' -> '\\'

literal :: Parser O.Literal
literal = try (O.String <$> stringLit)
        <|> try (O.Real <$> double)
        <|> try (O.Int <$> integer)
        <|> try (O.Bool True <$ string "#t")
        <|> try (O.Bool False <$ string "#f")
        <|> try (O.EmptyList <$ string "'()'")
        <|> try (O.Nil <$ string "nil")

regs :: O.Operator -> [O.Reg] -> A.Instr
regs op operands = A.ObjectCode (O.Regs op operands)

reg :: Parser Int
reg = read <$> (string "$r" *> many1 digit)

spc :: Parser a -> Parser a
spc p = space *> p

eR0 op = regs op []
eR1 op r1 = regs op [r1]
eR2 op r1 r2 = regs op [r1, r2]
eR3 op r1 r2 r3 = regs op [r1, r2, r3]
eR1LIT op r1 lit = A.ObjectCode (O.RegLit op r1 lit)
eR1GLO op r1 name = A.ObjectCode (O.RegGlo op r1 name)
eR1U16 op r1 u16 = A.ObjectCode (O.RegsInt op [r1] u16)
eR2U8 op r1 r2 u8 = A.ObjectCode (O.RegsInt op [r1, r2] u8)
eR0I24 op i24 = A.ObjectCode (O.RegsInt op [] i24)

oneOfStr :: [String] -> Parser String
oneOfStr strs = choice (map (try . string) strs)

singleLineInstr :: Parser A.Instr
singleLineInstr =
  -- standard cases
  try binop
  <|> try (regInstr eR3 A.opcodesR3 <*> spc reg <*> spc reg)
  <|> try (regInstr eR2 A.opcodesR2 <*> spc reg)
  -- eR1 do not share the assignment operator
  <|> try (eR1 <$> oneOfStr A.opcodesR1 <*> spc reg)
  <|> try (regInstr eR2U8 A.opcodesR2U8 <*> spc reg <*> spc integer)
  <|> try (regInstr eR1U16 A.opcodesR1U16 <*> spc integer)
  <|> try (eR0I24 <$> oneOfStr A.opcodesR0I24 <*> spc integer)
  <|> try (eR0 <$> oneOfStr A.opcodesR0)
  -- special cases
  <|> try (eR2 "copy" <$> reg <* spc (string ":=") <*> spc reg)
  <|> try (eR1 "zero" <$> reg <* spc (string ":=") <* spc (char '0'))
  <|> try (eR1LIT "loadliteral" <$> reg <* spc (string ":=") <*> spc literal)
  <|> try (eR1GLO "getglobal" <$> reg <* spc (string ":=") <*> (spc (string "G[") *> name <* string "]"))
  <|> try (flip (eR1GLO "setglobal") <$>
    (string "G[" *> name <* string "]") <* spc (string ":=") <*> spc reg)
  <|> try (regInstr eR1LIT ["popen"] <*> spc literal)
  <|> try (eR1LIT <$> oneOfStr ["check", "expect"] <*> spc reg <*> spc literal)
  <|> try (A.DefLabel <$> (string "def" *> spc name))
  <|> try (A.GotoLabel <$> (string "goto" *> spc name))
  <|> try (A.IfGotoLabel <$> (string "if" *> spc reg) <*> (spc (string "goto") *> spc name))
  where regInstr eRX opcodes = do
          r1 <- reg
          spc (string ":=")
          op <- spc (oneOfStr opcodes)
          return (eRX op r1)          -- doesn't quite fit eR1 format
        binop = do
          r1 <- reg
          spc (string ":=")
          r2 <- spc reg
          op <- spc (oneOfStr A.binops)
          r3 <- spc reg
          return (eR3 op r1 r2 r3)

--  <instruction> ::= <one_line_instruction> EOL
--                 |  <loadfunStart> {<instruction>} <loadfunEnd>

-- Design concrete syntax to mark the start of a “load function” instruction. 
-- This syntax must include the destination register into which the function will be loaded, 
-- plus the number of arguments that the function is expecting. 
-- If you have already implemented an unparser for the LOADFUNC form, start with that syntax.

-- Implement the loadfunStart parser. The marker for a function start can be as simple as a single token or as fancy as you like.

-- A “load function” instruction is followed by a sequence of assembly-language instructions, one per line. 
-- This sequence should be followed by some sort of closing delimiter, perhaps on a line by itself. 
-- Design concrete syntax for that delimiter, and implement it in the loadfunEnd parser.

-- The delimiter can be as simple as a single token or as fancy as you like. But it must not look like an instruction.

-- Create a test file loadfun.vs that exercises the new syntax.

-- Confirm that assembling the file generates a “load function” for the SVM:

-- uft vs-vo loadfun.vs | fgrep .load
-- Confirm that the function loads without error:3

-- uft vs-vo loadfun.vs | svm
-- You’ll confirm that it loads correctly later, after you’ve made a small extension to the SVM.

comment :: Parser ()
comment = () <$ spaces <* string ";;" <* manyTill anyChar endOfLine

spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

instruction :: Parser A.Instr
instruction = skippable *> (try singleLineInstr
            <|> try (A.LoadFunc <$> reg <* spc (string ":=") <*
              spc (string "fun") <*> spc integer <*>
              (spc (string "{") *> manyTill instruction loadFunEnd)))
              <* skippable
            where 
              loadFunEnd = string "}" <* newline
              skippable = (try (skipMany (spaced comment)) <|> spaces)

asmParse :: Parser [A.Instr]
asmParse = manyTill instruction eof