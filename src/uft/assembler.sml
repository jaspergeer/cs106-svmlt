(* Label elimination: translate assembly code into virtual object code *)

(* You'll complete this file *)

structure Assembler :>
  sig
    val translate : AssemblyCode.instr list -> ObjectCode.instr list Error.error
      (* What can go wrong: An undefined or multiply-defined label *)

  end
  =
struct

  structure A = AssemblyCode
  structure E = Env
  structure O = ObjectCode

  type 'a error = 'a Error.error
  val (succeed, <*>, <$>, >=>, >>=) = (Error.succeed, Error.<*>, Error.<$>, Error.>=>, Error.>>=)
  infixr 4 <$>
  infix 3  <*>
  infix 2  >=>
  infix 0 >>=
  val fail = Error.ERROR

  fun curry f x y = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun flip f x y  = f y x
  fun cons x xs = x :: xs

  (* A "translation" that cannot handle any labels.  You will define a better one *)
  fun translate instrs =
    let fun cvt (A.OBJECT_CODE instr)       = Error.OK instr
          | cvt (A.LOADFUNC (r, k, instrs)) = curry3 O.LOADFUNC r k <$> translate instrs
          | cvt _                           = Error.ERROR "assembler not implemented"
    in  Error.list (map cvt instrs)
    end

  (* In lab, define `fold`, `lift`, `labelEnv`, `labelElim`, and `translate` here *)
  (* val fold : (int * AssemblyCode.instr * 'a -> 'a) -> 'a -> AssemblyCode.instr list -> 'a *)

  fun fold f acc intrs = 
    let fun foldintr f acc [] count = acc
          | foldintr f acc (i::is) count =
              (case i of 
                  A.DEFLABEL _      => foldintr f (f count i acc) is count
                | A.IF_GOTO_LABEL _ => foldintr f (f count i acc) is (count+2)
                | _                 => foldintr f (f count i acc) is (count+1))
    in foldintr f acc intrs 0
    end

  (* val lift : ('a * 'b * 'c -> 'c error) -> ('a * 'b * 'c error -> 'c error) *)

  (* fun lift f = fn (a, b, c) => case c of
                                  Error.OK c' => f (a, b, c')
                                | Error.ERROR s => Error.ERROR s *)

  fun lift f (a, b, ce)= ce >>= (curry3 f a b)
  val _ = lift : ('a * 'b * 'c -> 'c error) -> ('a * 'b * 'c error -> 'c error)

  (* val labelEnv : AssemblyCode.instr list -> int Env.env error *)


  (* val labelEnv : AssemblyCode.instr list -> int Env.env error *)


  (* To define fold, you need to know exactly how much space each assembly instruction takes up in the final object code:

  A label definition takes no space.

  An IF_GOTO_LABEL takes two slots (one for if and one for goto).

  Every other instruction, including LOADFUNC, takes exactly one slot. (The LOADFUNC carries many instructions, but they go into a VMFunction that is added to the literal pool. As you know from your SVM, only a single load-literal instruction is emitted into the current instruction stream.) *)



end
