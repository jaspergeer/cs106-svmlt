module AsmParse where

import qualified Asm as A
import qualified ObjectCode as O
import qualified Data.Map as M
import qualified Data.Set as S
import qualified ParseUtils
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
      endOfLine, skipMany)
import Text.Parsec.Char (noneOf)

-- parsing

lexeme = ParseUtils.lexeme
name :: Parser String
name = lexeme $ (:) <$> oneOf (['a'..'z'] ++ ['A'..'Z']) <*> many alphaNum
int = ParseUtils.int
tok = ParseUtils.token
double = ParseUtils.double
manyTill' = ParseUtils.manyTill'
bool = ParseUtils.bool

stringLit :: Parser String
stringLit = lexeme $ char '"' *> manyTill' (try escape <|> noneOf "\n") (char '"')
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
literal = lexeme $ try (O.String <$> stringLit)
        <|> try (O.Real <$> double)
        <|> try (O.Int <$> int)
        <|> try (O.Bool <$> bool)
        <|> try (O.EmptyList <$ string "'()'")
        <|> try (O.Nil <$ string "nil")

regs :: O.Operator -> [O.Reg] -> A.Instr
regs op operands = A.ObjectCode (O.Regs op operands)

reg :: Parser Int
reg = lexeme $ read <$> (string "$r" *> many1 digit)

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
oneOfStr strs = lexeme $ choice (map (try . string) strs)

singleLineInstr :: Parser A.Instr
singleLineInstr =
  -- binops
  try ((\r1 r2 op r3 -> eR3 op r1 r2 r3) <$> reg <* setTo <*> reg <*> oneOfStr A.binops <*> reg)
  
  -- standard cases
  <|> try (flip eR3 <$> reg <* setTo <*> oneOfStr A.opcodesR3 <*> reg <*> reg)
  <|> try (flip eR2 <$> reg <* setTo <*> oneOfStr A.opcodesR2 <*> reg)
  <|> try (eR1 <$> oneOfStr A.opcodesR1 <*> reg)
  <|> try (eR0 <$> oneOfStr A.opcodesR0)

  <|> try (flip eR2U8 <$> reg <* setTo <*> oneOfStr A.opcodesR2U8 <*> reg <*> int)
  <|> try (flip eR1U16<$> reg <* setTo <*> oneOfStr A.opcodesR1U16 <*> int)
  <|> try (eR0I24 <$> oneOfStr A.opcodesR0I24 <*> int)

  -- special cases
  <|> try (A.DefLabel <$> (tok "def" *> name))
  <|> try (A.GotoLabel <$> (tok "goto" *> name))
  <|> try (A.IfGotoLabel <$> (tok "if" *> reg) <*> (tok "goto" *> name))

  <|> try (eR2 "copy" <$> reg <* setTo <*> reg)
  <|> try (eR1 "zero" <$> reg <* setTo <* tok "0")

  <|> try (flip eR1LIT <$> reg <* setTo <*> tok "popen" <*> literal)
  <|> try (eR1LIT "loadliteral" <$> reg <* setTo <*> literal)
  <|> try (eR1LIT <$> oneOfStr ["check", "expect"] <*> reg <*> literal)

  <|> try (eR1GLO "getglobal" <$> reg <* setTo <*> (tok "G[" *> name <* tok "]"))
  <|> try (flip (eR1GLO "setglobal") <$> (tok "G[" *> name <* tok "]") <* setTo <*> reg)
  where setTo = tok ":="

comment :: Parser ()
comment = () <$ tok ";;" <* manyTill anyChar endOfLine <* spaces

instruction :: Parser A.Instr
instruction =  try singleLineInstr
           <|> (A.LoadFunc <$> reg <* tok ":=" <* tok "fun" <*> int <*>
                                  (tok "{" *> manyTill instruction (tok "}")))

parse :: Parser [A.Instr]
parse = spaces *> skipMany comment *> 
  manyTill (instruction <* skipMany comment) eof