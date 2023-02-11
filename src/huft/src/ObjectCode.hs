module ObjectCode where
-- see norman's object-code.sml
data Literal
  = Int Integer
  | Real Double
  | String String
  | Bool Bool
  | EmptyList
  | Nil
  deriving Show

type Reg = Int
type Operator = String

data Instr
  = Regs Operator [Reg]
  | RegsLit Operator [Reg] Literal
  | Goto Int
  | LoadFunc Reg Int [Instr]
  | RegInt Operator Reg Reg Int
  | RegGlo Operator Reg String
  deriving Show

type Module = [Instr]