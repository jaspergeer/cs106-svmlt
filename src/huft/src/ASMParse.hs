module ASMParse where
import qualified ASM as A
import qualified ObjectCode as O
import Text.Parsec.ByteString (Parser)
import Data.List (intercalate)
import Text.Parsec ( digit, many1, (<|>), string, letter, alphaNum, char)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

-- parsing

natural :: Parser Integer
natural = read <$> many1 digit

integer :: Parser Integer
integer = negate <$ string "-" <*> natural
        <|> natural

literal :: Parser O.Literal
literal = O.Int <$> integer
        <|> O.Bool True <$ string "true"
        <|> O.Bool False <$ string "false"
        <|> O.EmptyList <$ string "emptylist"
        <|> O.Nil <$ string "nil"

-- unparsing

reg :: Show a => a -> [Char]
reg r = "r" ++ show r

concatSp :: [[Char]] -> [Char]
concatSp = intercalate " "

unparse1 :: A.Instr -> String
unparse1 i = case i of
  A.ObjectCode (O.Regs op regs) -> concatSp $ op : map reg regs
  A.DefLabel label -> concatSp ["def", label]
  A.GotoLabel label -> concatSp ["goto", label]
  A.IfGotoLabel r label -> concatSp ["if", reg r, "goto", label]
  _ -> error "unkown assembly instruction"

unparse :: [A.Instr] -> [String]
unparse (i:is) =
  case i of
    A.LoadFunc r arity body -> concat [".loadfunc" : (unparse body),
                                       ".endload" : (unparse is)]
    _ -> unparse1 i : unparse is
unparse [] = []