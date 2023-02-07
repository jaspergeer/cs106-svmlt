module ASMParse where
import Control.Monad
import qualified ASM as A
import qualified ObjectCode as O
import Text.Parsec.String ( Parser )
import Data.List (intercalate)
import Text.Parsec ( digit, many1, (<|>), string, letter, alphaNum, char, option, oneOf, spaces, many, manyTill, space, newline, choice, parserFail, (<?>), parserZero, try)
import qualified Text.Parsec.Token as T
import qualified Data.Map as M
import Text.Parsec.Error (newErrorMessage, Message (Expect))

-- parsing

line :: Parser a -> Parser a
line p = p <* newline

integer :: Parser Integer
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

nat :: Parser Int
nat = read <$> many1 digit

double :: Parser Double
double = read <$> ((++) <$> whole <*> decimal)
      where whole = (++) <$> option "" (string "-") <*> many1 digit
            decimal = (++) <$> string "." <*> many1 digit

literal :: Parser O.Literal
literal = try (O.Real <$> try double)
        <|> try (O.Int <$> integer)
        <|> try (O.Bool True <$ string "true")
        <|> try (O.Bool False <$ string "false")
        <|> try (O.EmptyList <$ string "emptylist")
        <|> try (O.Nil <$ string "nil")

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

type Short = String

binopTableInit :: [(Short, O.Operator)]
binopTableInit = 
  [("+", "add")
    , ("-",  "sub")
    , ("*",  "mul")
    , ("/",  "div")]

binopShorts :: [Short]
binopShorts = map fst binopTableInit

binopTable :: M.Map Short O.Operator
binopTable = foldr (\(x, y) m -> M.insert x y m)  M.empty binopTableInit

binopShort :: Parser O.Operator
binopShort = do
  short <- choice (map string binopShorts)
  case (M.lookup short binopTable) of
    Just op -> return op
    _ -> parserZero <?> "a binary operator"

singleLineInstr :: Parser A.Instr
singleLineInstr = (line $
  binop
  <|> try (eR3 <$> opcode <*> spc reg <*> spc reg <*> spc reg)
  <|> try (eR2 <$> opcode <*> spc reg <*> spc reg)
  <|> try (eR1LIT <$> opcode <*> spc reg <* spc (string "lit")) <*> spc literal
  <|> try (eR1 <$> opcode <*> spc reg)
  <|> try (eR0 <$> opcode))
  <?> "a single line assembly instruction"
  where binop = do
          r1 <- reg
          spc (string ":=")
          r2 <- spc reg
          op <- spc binopShort
          r3 <- spc reg
          return $ eR3 op r1 r2 r3


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