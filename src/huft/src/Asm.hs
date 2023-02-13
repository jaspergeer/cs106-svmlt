-- merged norman's asm.sml, asmlex.sml, and probably asmparse.sml

module Asm where

import qualified ObjectCode as O
import qualified Data.Set as S

type Label = String

data Instr
  = ObjectCode O.Instr
  | LoadFunc O.Reg Int [Instr]
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
  , "mod"
  , "idiv"
  , "and"
  , "or"
  , "xor"
  , "n="
  , "s="
  , ">"
  , "<"
  , ">="
  , "<=" ]

opcodesR3 =
  [ "cons" ]

opcodesR2 =
  [ "truth"
  , "number?"
  , "not"
  , "car"
  , "cdr"
  , "function?"
  , "pair?"
  , "symbol?"
  , "boolean?"
  , "null?"
  , "nil?"
  , "hash" ]

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

opcodesR1U16 =
  []

opcodesR2U8 =
  []

opcodesR0 =
  [ "halt"]

opcodes = concat
  [ opcodesR3
  , opcodesR2
  , opcodesR1
  , opcodesR0
  , opcodesR1LIT
  , opcodesR1GLO ]