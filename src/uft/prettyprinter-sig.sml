(* Part of the Pretty Printer *)

(* You can ignore this *)

signature PRETTYPRINTER = sig
  val print : {width : int, print : string -> unit} -> PP.pretty -> unit
end
