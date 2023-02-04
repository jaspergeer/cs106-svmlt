module ObjectCode where

data Literal
  = Int Int
  | Real Double
  | String String
  | EmptyList
  | Nil

type Reg = Int
type Operator = String

data Instr
  = Regs Operator [Reg]
  | RegsLit Operator [Reg] Literal
  | Goto Int
  | LoadFunc Reg Int [Instr]
  | RegInt Operator Reg Reg Int