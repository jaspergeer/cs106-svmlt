module ASMParse where
import Control.Monad
import qualified ASM as A
import qualified ObjectCode as O
import Text.Parsec.String ( Parser )
import Data.List (intercalate)
import Text.Parsec ( digit, many1, (<|>), string, letter, alphaNum, char, option, oneOf, spaces, many, manyTill, space, newline)
import qualified Text.Parsec.Token as T

-- parsing

line :: Parser a -> Parser a
line p = p <* newline

integer :: Parser Integer
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

nat :: Parser Int
nat = read <$> many1 digit

double :: Parser Double
double = read <$> ((++) <$> whole <*> decimal)
      <|> fromIntegral <$> integer
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

literal :: Parser O.Literal
literal = O.Int <$> integer
        <|> O.Real <$> double
        <|> O.Bool True <$ string "true"
        <|> O.Bool False <$ string "false"
        <|> O.EmptyList <$ string "emptylist"
        <|> O.Nil <$ string "nil"

regs :: O.Operator -> [O.Reg] -> A.Instr
regs op operands = A.ObjectCode (O.Regs op operands)

opcode :: Parser String
opcode = many1 alphaNum

reg :: Parser Int
reg = read <$> (char 'r' *> many1 digit)

spc :: Parser a -> Parser a
spc p = space *> p

eR0 op = regs op []
eR1 op r1 = regs op [r1]
eR2 op r1 r2 = regs op [r1, r2]
eR3 op r1 r2 r3 = regs op [r1, r2, r3]
eR1LIT op r1 lit = A.ObjectCode (O.RegsLit op [r1] lit)

singleLineInstr :: Parser A.Instr
singleLineInstr = line $
  eR3 <$> opcode <*> spc reg <*> spc reg <*> spc reg
  <|> eR2 <$> opcode <*> spc reg <*> spc reg
  <|> eR1 <$> opcode <*> spc reg
  <|> eR0 <$> opcode
  <|> eR1LIT <$> opcode <*> spc reg <* spc (string "lit") <*> spc literal

loadFunc arity body x = A.LoadFunc x arity body

instruction :: Parser A.Instr
instruction = singleLineInstr
            <|> loadFunBegin <*> manyTill instruction loadFunEnd
            where loadFunBegin = line $ do
                    string ".loadfunc"
                    arity <- spc nat
                    return (\body -> loadFunc arity body (length body))
                  loadFunEnd = line $ string ".loadend"