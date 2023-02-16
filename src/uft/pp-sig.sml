(* Part of the Pretty Printer *)

(* You can ignore this *)

signature PP = sig
  datatype break = OPTIONAL | CONNECTED | FORCED
  datatype pretty = BEGIN 
                  | END 
                  | INDENT of int 
                  | OUTDENT 
                  | BREAK of {break: break, pre:string, post:string, none:string}
                  | TEXT of string 
                  | LIST of pretty list
                  | SYNCH
  type break_info = {break: break, pre:string, post:string, none:string}
  structure Short : sig
    val be : pretty
    val en : pretty
    val i  : int -> pretty
    val ou : pretty
    val nl : pretty
    val on : pretty
    val cn : pretty
    val te : string -> pretty
    val li : pretty list -> pretty
    val sy : pretty
    val $  : string -> pretty   (* implement $ escapes *)
    val int : int -> pretty
    val char : char -> pretty
  end
  val format : string -> pretty list -> pretty  (* $ escapes plus % escapes *)
  val textMap : (string -> string) -> pretty -> pretty
  val flatten : pretty -> string
  val synch : pretty
end
