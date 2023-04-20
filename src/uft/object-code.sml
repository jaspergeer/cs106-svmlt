(* A basic representation of object code *)

(* You'll need to understand what's going on here and how it's used *)

structure ObjectCode = struct
    datatype literal = INT of int
                     | REAL of real
                     | STRING of string
                     | BOOL of bool
                     | EMPTYLIST
                     | NIL

    type reg = int
    type operator = string (* opcode name recognized by the VM loader *)

    datatype instr
      = REGS    of operator * reg list
      | REGSLIT of operator * reg list * literal
      | GOTO    of int             (* PC-relative branches *)
      | LOADFUNC of reg * int * instr list
              (* LOADFUNC (r, k, body) means:
                    - body describes a function
                      that expects k parameters
                    - capture those instructions and insert the function
                      into the literal pool
                    - emit an instruction to load that literal into register r
               *)

      | REGINT     of operator * reg * reg * int
      | GOTO_VCON  of reg * int (* number of succeeding jump-table entries *)
      | JUMP_TABLE_ENTRY of (int * int * literal)
             (* (arity, PC-relative offset, vcon value) *)


    type module = instr list

end


structure ObjectUnparser :> sig
  (* emit on-disk form of virtual object code *)
  val module : ObjectCode.instr list -> string list 
     (* emits ".load module" with the right size *)

  val literal : ObjectCode.literal -> string list 
    (* tokens of a literal in virtual object code *)
end
  =
struct
  structure O = ObjectCode
  val concatSp = String.concatWith " "
  val fixSign = String.map (fn #"~" => #"-" | c => c) 
  val int = fixSign o Int.toString

  fun literal (O.INT n) = [int n]
    | literal (O.REAL x) = [fixSign (Real.toString x)]
    | literal (O.BOOL b) = [if b then "true" else "false"]
    | literal (O.EMPTYLIST) = ["emptylist"]
    | literal (O.NIL) = ["nil"]
    | literal (O.STRING s) =
        let val codes = (map Char.ord o explode) s
        in  "string" :: int (length codes) :: map int codes
        end

  fun instr (O.REGS (opr, rs))   = concatSp (opr :: map int rs)
    | instr (O.REGSLIT (opr, rs, v))   = concatSp (opr :: map int rs @ literal v)
    | instr (O.GOTO offset) = concatSp ["goto", int offset]
    | instr (O.REGINT (opr, r1, r2, offset)) =
               concatSp [opr, int r1, int r2, int offset]
    | instr (O.LOADFUNC _) = Impossible.impossible "LOADFUNC reached instr"
    | instr (O.GOTO_VCON (r, n)) = concatSp ["goto-vcon", int r, int n]
    | instr (O.JUMP_TABLE_ENTRY (arity, offset, con)) =
        concatSp ("jump-table-entry" :: int arity :: int offset :: literal con)

  fun add (O.LOADFUNC (r, k, body), tail) =
        list (concatSp [".load", int r, "function", int k]) body tail
    | add (i, tail) = instr i :: tail
  and list prefix body tail =
        concatSp [prefix, int (length body)] :: foldr add tail body

  fun module code = list ".load module" code []

end
