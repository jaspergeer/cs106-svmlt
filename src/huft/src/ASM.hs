-- merged norman's asm.sml, asmlex.sml, and probably asmparse.sml

module ASM where

import qualified ObjectCode as O
import qualified Data.Set as S

type Label = String
type Arity = Int

data Instr
  = ObjectCode O.Instr
  | LoadFunc O.Reg Arity [Instr]
  | DefLabel Label
  | GotoLabel Label
  | IfGotoLabel O.Reg Label
  deriving Show

type Binop = String
type Unop = String

binops =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "&&"
  , "||"
  , "^"
  , "=="
  , ">"
  , "<"
  , ">="
  , "<=" ]

opcodesR3 =
  [ "cons" ]

opcodesR2 =
  [ "truth"
  , "not"
  , "car"
  , "cdr" ]

opcodesR1 =
  [ "print"
  , "println"
  , "printu"
  , "dload"
  , "cskip" ]

opcodesR1LIT =
  [ "popen"
  , "loadliteral"
  , "check"
  , "expect" ]

opcodesR1GLO =
  [ "getglobal"
  , "setglobal" ]

opcodesR0I24 =
  [ "jump" ]

opcodesR0 =
  [ "halt"]

opcodes = concat
  [ opcodesR3
  , opcodesR2
  , opcodesR1
  , opcodesR0
  , opcodesR1LIT
  , opcodesR1GLO ]