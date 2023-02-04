-- merged norman's asm.sml, asmlex.sml, and probably asmparse.sml

module ASM (Instr) where

data Instr
  = ObjectCode
  | LoadFunc
  | DefLabel String
  | GotoLabel String
  | IfGotoLabel



-- (* A basic representation of assembly code: 
--     it's essentially object code, but with labels. *)

-- (* You'll need to understand what's going on here, and how it's used *)

-- structure AssemblyCode = struct
--   (* A valid object-code instruction is also a valid assembly-code instruction.
--      Instruction forms with labels appear only in assembly code. *)

--   datatype instr
--     = OBJECT_CODE of ObjectCode.instr                   (* an object-code instruction *)
--     | LOADFUNC    of ObjectCode.reg * int * instr list  (* body is assembly code *)

--     | DEFLABEL      of string
--     | GOTO_LABEL    of string
--     | IF_GOTO_LABEL of ObjectCode.reg * string
-- end
