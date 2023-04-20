(* Representation of VScheme, with some utility functions *)

(* You'll need to understand what's going on and how it's used. *)

structure VScheme = struct
  type name = string

  type pat = Pattern.pat

  datatype exp = LITERAL of value
               | VAR     of name
               | SET     of name * exp
               | IFX     of exp * exp * exp
               | WHILEX  of exp * exp
               | BEGIN   of exp list
               | APPLY   of exp * exp list
               | LETX    of let_kind * (name * exp) list * exp
               | LAMBDA  of lambda
               | COND of (exp * exp) list  (* for embedding only *)
               | VCON of Pattern.vcon
               | CASE of exp Case.t

  and let_kind = LET | LETREC
  and    value = SYM       of name
               | INT       of int
               | REAL      of real
               | BOOLV     of bool   
               | PAIR      of value * value
               | EMPTYLIST
    withtype lambda = name list * exp

  datatype def  = VAL    of name * exp
                | DEFINE of name * lambda
                | EXP    of exp
                | CHECK_EXPECT of exp * exp
                | CHECK_ASSERT of exp

end

structure UnambiguousVScheme = struct
  type name = string
  type primitive = Primitive.primitive
  datatype let_kind = datatype VScheme.let_kind
  datatype exp = LITERAL   of value
               | LOCAL     of name
               | GLOBAL    of name
               | SETLOCAL  of name * exp
               | SETGLOBAL of name * exp
               | IFX       of exp * exp * exp
               | WHILEX    of exp * exp
               | BEGIN     of exp list
               | FUNCALL   of exp * exp list
               | PRIMCALL  of primitive * exp list
               | LETX      of let_kind * (name * exp) list * exp
               | LAMBDA    of lambda
               | CASE        of exp Case.t
               | CONSTRUCTED of exp Constructed.t
  and    value = SYM       of name
               | INT       of int
               | REAL      of real
               | BOOLV     of bool   
               | EMPTYLIST
    withtype lambda = name list * exp

  datatype def  = VAL    of name * exp
                | DEFINE of name * lambda
                | EXP    of exp
                | CHECK_EXPECT of string * exp * string * exp
                | CHECK_ASSERT of string * exp

  fun valToString (SYM x) = x
    | valToString (INT n) = Int.toString n
    | valToString (REAL x) = Real.toString x
    | valToString (BOOLV b) = Bool.toString b
    | valToString (EMPTYLIST) = "'()"

  fun whatIs (LITERAL v)        = "LITERAL " ^ valToString v
    | whatIs (LOCAL x)          = "LOCAL " ^ x
    | whatIs (GLOBAL x)         = "GLOBAL " ^ x
    | whatIs (SETLOCAL (x, e))  = "SETLOCAL " ^ x
    | whatIs (SETGLOBAL (x, e)) = "SETGLOBAL " ^ x
    | whatIs (IFX (e1, e2, e3)) = "IFX"
    | whatIs (WHILEX (e1, e2))  = "WHILEX"
    | whatIs (BEGIN es)         = "BEGIN"
    | whatIs (FUNCALL (e, es))  = "FUNCALL"
    | whatIs (PRIMCALL (x, es)) = "PRIMCALL " ^ Primitive.name x
    | whatIs (LETX (LET, bs, body))           = "LET"
    | whatIs (LETX (LETREC, bindings, body))  = "LETREC"
    | whatIs (LAMBDA (xs, e))                 = "LAMBDA"
    | whatIs (CASE _) = "CASE"
    | whatIs (CONSTRUCTED _) = "CONSTRUCTED"

end

structure VSchemeTests : sig
  val delay : VScheme.def list -> VScheme.def list  (* put tests at end *)
end
  =
struct
  structure S = VScheme
  fun isTest (S.CHECK_ASSERT _) = true
    | isTest (S.CHECK_EXPECT _) = true
    | isTest (S.VAL _) = false
    | isTest (S.DEFINE _) = false
    | isTest (S.EXP _) = false

  fun delay ds =
    let val (tests, other) = List.partition isTest ds
    in  other @ tests
    end
end

structure VSchemeUtils : sig
  type exp = VScheme.exp
  val car : exp -> exp
  val cdr : exp -> exp
  val cons : exp -> exp -> exp
  val list : exp list -> exp
  val nth : int -> exp -> exp
  val setnth : exp -> int -> exp -> exp

  val setcar : exp -> exp -> exp

  val constructed : Pattern.vcon -> exp list -> exp
  val block : exp list -> exp
  val switchVcon : exp -> (VScheme.value * exp) list -> exp option -> exp
  val withJoinPoints : exp list -> exp -> exp
  val joinAt : int -> exp

end
  =
struct
  structure S = VScheme

  type exp = VScheme.exp

  fun car e = S.APPLY (S.VAR "car", [e])
  fun cdr e = S.APPLY (S.VAR "cdr", [e])
  fun cons x xs = S.APPLY (S.VAR "cons", [x, xs])
  fun setcar e v = S.APPLY (S.VAR "set-car!", [e, v])

  fun nth 0 e = car e
    | nth k e = nth (k-1) (cdr e)

  fun list [] = S.LITERAL S.EMPTYLIST
    | list (e::es) = cons e (list es)

  fun setnth e 0 v = S.APPLY (S.VAR "set-car!", [e, v])
    | setnth e k v = setnth (cdr e) (k - 1) v

  fun constructed con es = S.APPLY (S.VCON con, es)

  fun switchVcon e choices default =
    let fun vpat (S.SYM con) = Pattern.APPLY (con, [])
          | vpat (S.INT k) = Pattern.INT k
          | vpat _ = Impossible.impossible "bad value in SWITCH_VCON"
        fun choice (value, e) = (vpat value, e)
        val last = case default
                    of NONE => []
                     | SOME e => [(Pattern.WILDCARD, e)]
    in  S.CASE (e, map choice choices @ last)
    end

  fun joinAt k = S.APPLY (S.VAR "$join", [S.LITERAL (S.INT k)])

  fun withJoinPoints es e =
    let val join_case =
          S.CASE (S.VAR "n", ListUtil.mapi (fn (i, e) => (Pattern.INT i, e)) es)
        val join_fun = S.LAMBDA (["n"], join_case)
    in  S.LETX (S.LET, [("$join", join_fun)], e)
    end

  fun block [] = Impossible.impossible "empty block"
    | block (e :: es) = S.APPLY (e, es)

end
