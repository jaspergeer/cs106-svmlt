(* Representation of Closed Scheme, with some utility functions *)

(* You'll need to understand what's going on and how it's used *)
(* You'll need to add an embedding for closures *)

structure ClosedScheme = struct
  (* extends first-order Scheme with three new forms *)
  type name = string
  datatype literal = datatype ObjectCode.literal
  datatype exp
    = LITERAL of literal
    | LOCAL     of name
    | GLOBAL    of name
    | IFX       of exp * exp * exp
    | PRIMCALL  of Primitive.primitive * exp list
    | FUNCALL   of exp * exp list
    | LET       of (name * exp) list * exp
    | BEGIN     of exp list
    | SETLOCAL  of name * exp
    | SETGLOBAL of name * exp
    | WHILEX    of exp * exp
  (* these are the new forms: *)
    | CAPTURED  of int
    | CLOSURE   of closure
    | LETREC    of (name * closure) list * exp
    | CONSTRUCTED of exp Constructed.t
    | CASE        of exp Case.t
  withtype closure = (name list * exp) * exp list (* funcode, captured variables *)
  type funcode = name list * exp (* invariant: has no free (LOCAL) variables *)

  datatype def  = VAL    of name * exp
                | DEFINE of name * funcode
                | EXP    of exp
                | CHECK_EXPECT of string * exp * string * exp
                | CHECK_ASSERT of string * exp
end


structure CSUtil :> sig
  val embed    : ClosedScheme.def -> VScheme.def
  val embedExp : ClosedScheme.exp -> VScheme.exp
    (* exploit closure conversion; runnable *)
end
  = 
struct
  structure C = ClosedScheme
  structure P = Primitive
  structure S = VScheme
  structure SU = VSchemeUtils

  (* This is a copy of function `FOUtil.embed` from file `foutil.sml`,
     with the three new cases added *)

  fun exp (C.CAPTURED i) =
        (* The embedding of a `CAPTURED` form should call the predefined
           vScheme function `CAPTURED-IN`.
         *)
        Impossible.exercise "embed reference to captured variable"
    | exp (C.CLOSURE ((formals, body), captured)) =
        (* The embedding of a `CLOSURE` form should call the primitive
           vScheme function `mkclosure`.
         *)
        Impossible.exercise "embed closure"
    | exp (C.LETREC  (bs, e))  =
        (* I've done this one *)
        S.LETX (S.LETREC,  map (fn (f, c) => (f, exp (C.CLOSURE c))) bs, exp e)

    | exp (C.FUNCALL (f, es))  = S.APPLY (exp f, map exp es)
    | exp (C.PRIMCALL (p, es)) = S.APPLY (S.VAR (P.name p), map exp es)
    | exp (C.LITERAL v)        = S.LITERAL (KNEmbed.value v)
    | exp (C.LOCAL x)          = S.VAR x
    | exp (C.GLOBAL x)         = S.VAR x
    | exp (C.IFX (e1, e2, e3)) = S.IFX (exp e1, exp e2, exp e3)
    | exp (C.LET     (bs, e))  = S.LETX (S.LET,     map binding bs, exp e)
    | exp (C.BEGIN es)         = S.BEGIN (map exp es)
    | exp (C.SETLOCAL (x, e))  = S.SET (x, exp e)
    | exp (C.SETGLOBAL (x, e)) = S.SET (x, exp e)
    | exp (C.WHILEX (c, body)) = S.WHILEX (exp c, exp body)
    | exp (C.CASE c)           = S.CASE (Case.map exp c)
    | exp (C.CONSTRUCTED (con, es)) = SU.constructed con (map exp es)
  and binding (x, e) = (x, exp e)

  fun def (C.EXP e) = S.EXP (exp e)
    | def (C.VAL (x, e)) = S.VAL (x, exp e)
    | def (C.DEFINE (f, lambda)) = S.VAL (f, exp (C.CLOSURE (lambda, [])))
    | def (C.CHECK_EXPECT (s, e, s', e')) = S.CHECK_EXPECT (exp e, exp e')
    | def (C.CHECK_ASSERT (s, e)) = S.CHECK_ASSERT (exp e)

  val embed = def
  val embedExp = exp

end
