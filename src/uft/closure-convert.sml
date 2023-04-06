(* Closure conversion from unambiguous VScheme to Closed Scheme. 
    This is where we handle lambda and captured variables *)

(* You'll write this file *)

structure ClosureConvert :> sig
  val close : UnambiguousVScheme.def -> ClosedScheme.def
end 
= 
struct
  structure X = UnambiguousVScheme
  structure C = ClosedScheme
  structure S = Set

  fun literal (X.SYM x)   = C.STRING x
    | literal (X.INT i)   = C.INT i
    | literal (X.REAL i)  = C.REAL i
    | literal (X.BOOLV b) = C.BOOL b
    | literal X.EMPTYLIST = C.EMPTYLIST


  fun indexOf x xs = 
    (* returns `SOME i`, where i is the position of `x` in `xs`,
       or if `x` does not appear in `xs`, returns `NONE` *)
    let fun find k []        = NONE 
          | find k (y :: ys) = if x = y then SOME k else find (k + 1) ys
    in  find 0 xs
    end

  fun closeExp captured e =
    (* Given an expression `e` in Unambiguous vScheme, plus a list
       of the free variables of that expression, return the closure-
       converted version of the expression in Closed Scheme *)
    let val _ = closeExp : X.name list -> X.exp -> C.exp

        (* I recommend internal function closure : X.lambda -> C.closure *)
        fun closure (xs, body) = 
              raise Impossible.exercise "closure-conversion of a `lambda`"
        val _ = closure : X.lambda -> C.closure

        (* I recommend internal function exp : X.exp -> C.exp *)
        fun exp _ = Impossible.exercise "close exp over `captured`"
    in  exp e
    end

  fun close def = Impossible.exercise "close a definition"


end
