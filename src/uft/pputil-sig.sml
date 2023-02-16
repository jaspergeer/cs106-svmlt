(* Part of the Pretty Printer *)

(* You can ignore this *)

signature PP_UTIL = sig
  val interleave : string * (PP.pretty * string) list -> PP.pretty
  val mixin : int -> PP.pretty * PP.pretty * PP.pretty -> PP.pretty list -> PP.pretty
  val newlines : PP.pretty list -> PP.pretty
  val lispo :   string * PP.pretty list -> PP.pretty
  val lispc :   string * PP.pretty list -> PP.pretty
  val lisp  : PP.pretty -> string * PP.pretty list -> PP.pretty
  val commaSeparate : PP.pretty -> PP.pretty list -> PP.pretty
end
