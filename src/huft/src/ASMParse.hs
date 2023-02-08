module ASMParse where
import qualified ASM as A
import qualified ObjectCode as O
import Text.Parsec.String ( Parser )
import Data.List (intercalate)
import Text.Parsec ( digit, many1, (<|>), string, letter, alphaNum, char, option, oneOf, spaces, many, manyTill, space, newline, choice, parserFail, (<?>), parserZero, try, anyChar, getPosition)
import qualified Text.Parsec.Token as T
import qualified Data.Map as M
import qualified Data.Set as S
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
eR1LIT op r1 lit = A.ObjectCode (O.RegsLit op [r1] lit)

type Short = String

binopTableInit :: [(Short, A.Binop)]
binopTableInit = 
  [("+", "add")
    , ("-",  "sub")
    , ("*",  "mul")
    , ("/",  "div")]

binopTable :: M.Map Short A.Binop
binopTable = foldr (\(x, y) m -> M.insert x y m)  M.empty binopTableInit

oneOfStr :: [String] -> Parser String
oneOfStr strs = choice (map string strs)

binopOpcode :: Parser O.Operator
binopOpcode = do
  short <- oneOfStr A.binops
  case M.lookup short binopTable of
    Just op -> return op
    _ -> error "IMPOSSIBLE: binop table and list inconsistent"

singleLineInstr :: Parser A.Instr
singleLineInstr = line
  (try binop
  <|> try (regInstr eR3 A.opcodesR3 <*> spc reg <*> spc reg)
  <|> try (regInstr eR2 A.opcodesR2 <*> spc reg)
  <|> try (regInstr eR1LIT A.opcodesR1LIT <*> spc literal)
  <|> try (regInstr eR1 A.opcodesR1)
  <|> try (eR0 <$> oneOfStr A.opcodesR0))
  -- special form for setzero??
  where regInstr eRX opcodes = do
          r1 <- reg
          spc (string ":=")
          op <- spc (oneOfStr opcodes)
          return (eRX op r1)
        binop = do
          r1 <- reg
          spc (string ":=")
          r2 <- spc reg
          op <- spc binopOpcode
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