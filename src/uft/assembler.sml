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
  val (succeed, <*>, <$>, >=>) = (Error.succeed, Error.<*>, Error.<$>, Error.>=>)
  infixr 4 <$>
  infix 3  <*>
  infix 2  >=>
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

end
