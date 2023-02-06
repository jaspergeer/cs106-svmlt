module ASMParse where
import Control.Monad
import qualified ASM as A
import qualified ObjectCode as O
import Text.Parsec.ByteString (Parser)
import Data.List (intercalate)
import Text.Parsec ( digit, many1, (<|>), string, letter, alphaNum, char, option, oneOf, spaces)
import qualified Text.Parsec.Token as T

-- parsing

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

natural :: Parser Integer
natural = read <$> many1 digit

integer :: Parser Integer
integer = negate <$ char '-' <*> natural
        <|> natural

double :: Parser Double
double = read <$> ((++) <$> whole <*> decimal)
      <|> fromIntegral <$> integer
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

literal :: Parser O.Literal
literal = lexeme $
        O.Int <$> integer
        <|> O.Real <$> double
        <|> O.Bool True <$ string "true"
        <|> O.Bool False <$ string "false"
        <|> O.EmptyList <$ string "emptylist"
        <|> O.Nil <$ string "nil"

regs :: O.Operator -> [O.Reg] -> A.Instr
regs op operands = A.ObjectCode (O.Regs op operands)

opcode :: Parser String
opcode = lexeme $ many1 alphaNum

reg :: Parser Int
reg = lexeme $ read <$> (char 'r' *> many1 digit)

eR0 op = regs op []
eR1 op r1 = regs op [r1]
eR2 op r1 r2 = regs op [r1, r2]
eR3 op r1 r2 r3 = regs op [r1, r2, r3]
eR1LIT op r1 lit = A.ObjectCode (O.RegsLit op [r1] lit)

singleLineInstr :: Parser A.Instr
singleLineInstr = eR0 <$> opcode
                <|> eR1 <$> opcode <*> reg 
                <|> eR2 <$> opcode <*> reg <*> reg
                <|> eR3 <$> opcode <*> reg <*> reg <*> reg
                <|> eR1LIT <$> opcode <*> reg <*> literal
                