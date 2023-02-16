(* Part of the Pretty Printer *)

(* You can ignore this *)

signature PP_COST = sig
  type cost
  val initialCost : cost (* cost of empty output *)
  val addNewline  : cost * int * string list -> cost (* cost of another newline *)
     (* cost of prev break, depth, parts of line in reverse order *)
  val < : cost * cost -> bool
end
signature PP_DYNAMIC = sig
  type emitter = int * string list -> unit
  type syncher = emitter * int -> int
  val set : emitter * syncher * int -> PPNormal.normal -> int
  val standardEmitLine : TextIO.outstream -> int * string list -> unit
  val hoEmit : (string * 'a -> 'a) -> 'a ref -> int * string list -> unit
end
