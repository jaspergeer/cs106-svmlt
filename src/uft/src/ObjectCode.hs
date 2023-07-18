module ObjectCode where
-- see norman's object-code.sml
data Literal
  = Int Int
  | Real Double
  | String String
  | Bool Bool
  | EmptyList
  | Nil
  deriving Show

type Reg = Int
type Operator = String

--   LOADFUNC (r, k, body) means:
--       - body describes a function
--         that expects k parameters
--       - capture those instructions and insert the function
--         into the literal pool
--       - emit an instruction to load that literal into register r
-- 
data Instr
  = Regs Operator [Reg]
  | RegLit Operator Reg Literal
  | Goto Int
  | LoadFunc Reg Int [Instr]
  | RegsInt Operator [Reg] Int
  | RegGlo Operator Reg String
  deriving Show

type Module = [Instr]