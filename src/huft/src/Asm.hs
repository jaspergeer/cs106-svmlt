module Asm where

import qualified ObjectCode as O

type Label = String

data Instr
  = ObjectCode O.Instr
  | LoadFunc O.Reg Int [Instr]
  | DefLabel Label
  | GotoLabel Label
  | IfGotoLabel O.Reg Label
  deriving Show

type Binop = String

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
  , "hash"
  , "copy" ]

opcodesR1 =
  [ "print"
  , "println"
  , "printu"
  , "dload"
  , "cskip"
  , "err" ]

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