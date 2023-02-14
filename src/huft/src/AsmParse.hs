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

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

word :: String -> Parser String
word w = lexeme $ string w

name :: Parser String
name = lexeme $ (:) <$> oneOf (['a'..'z'] ++ ['A'..'Z']) <*> many alphaNum

integer :: Parser Int
integer = lexeme $ read <$> ((++) <$> option "" (string "-") <*> many1 digit)

nat :: Parser Int
nat = lexeme $ read <$> many1 digit

double :: Parser Double
double = lexeme $ read <$> ((++) <$> whole <*> decimal)
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

-- same behavior as manyTill except first parser is tried before the second
manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p1 p2 = try ((:) <$> p1 <*> manyTill' p1 p2) <|> ([] <$ p2)

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
        <|> try (O.Int <$> integer)
        <|> try (O.Bool True <$ string "#t")
        <|> try (O.Bool False <$ string "#f")
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
oneOfStr strs = choice (map (try . string) strs)

singleLineInstr :: Parser A.Instr
singleLineInstr =
  -- binops
  try ((\r1 r2 op r3 -> eR3 op r1 r2 r3) <$> reg <* setTo <*> reg <*> oneOfStr A.binops <*> reg)
  -- standard cases
  <|> try (flip eR3 <$> reg <* setTo <*> oneOfStr A.opcodesR3 <*> reg <*> reg)
  <|> try (flip eR2 <$> reg <* setTo <*> oneOfStr A.opcodesR2 <*> reg)
  -- eR1 does not share the assignment operator
  <|> try (eR1 <$> oneOfStr A.opcodesR1 <*> reg)
  <|> try (flip eR2U8 <$> reg <* setTo <*> oneOfStr A.opcodesR2U8 <*> reg <*> integer)
  <|> try (flip eR1U16<$> reg <* setTo <*> oneOfStr A.opcodesR1U16 <*> integer)
  <|> try (eR0I24 <$> oneOfStr A.opcodesR0I24 <*> integer)
  <|> try (eR0 <$> oneOfStr A.opcodesR0)
  -- special cases
  <|> try (A.DefLabel <$> (string "def" *> name))
  <|> try (A.GotoLabel <$> (string "goto" *> name))
  <|> try (A.IfGotoLabel <$> (string "if" *> reg) <*> (word "goto" *> name))
  <|> try (eR2 "copy" <$> reg <* setTo <*> reg)
  <|> try (eR1 "zero" <$> reg <* setTo <* word "0")
  <|> try (eR1LIT "loadliteral" <$> reg <* setTo <*> literal)
  <|> try (flip eR1LIT <$> reg <* setTo <*> word "popen" <*> literal)
  <|> try (eR1LIT <$> oneOfStr ["check", "expect"] <*> reg <*> literal)
  <|> try (eR1GLO "getglobal" <$> reg <* setTo <*> (word "G[" *> name <* word "]"))
  <|> try (flip (eR1GLO "setglobal") <$> (word "G[" *> name <* string "]") <* setTo <*> reg)
  where setTo = word ":="

comment :: Parser ()
comment = () <$ spaces <* string ";;" <* manyTill anyChar endOfLine

instruction :: Parser A.Instr
instruction = skippable *> 
              (try singleLineInstr
           <|> try (A.LoadFunc <$> reg <* word ":=" <* word "fun" <*> integer <*>
                                  (word "{" *> manyTill instruction (word "}"))))
              <* skippable
            where 
              skippable = try (skipMany (lexeme comment)) <|> spaces

asmParse :: Parser [A.Instr]
asmParse = manyTill instruction eof