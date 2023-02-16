(* Disambiguates names in source code:
     - Names of local variables and parameters
     - Global names of primitive functions
     - All other global names
   Also can embed disambiguated code back into ambiguous code. *)

(* You'll complete the disambiguator and ambiguator *)

structure Disambiguate :> sig
  val disambiguate : VScheme.def -> UnambiguousVScheme.def
     (* N.B. Also eta-expands primitives in non-application position *)
  val ambiguate : UnambiguousVScheme.def -> VScheme.def
  val ambiguateExp : UnambiguousVScheme.exp -> VScheme.exp
end
  =
struct
  structure P = Primitive
  structure S = VScheme
  structure X = UnambiguousVScheme   (* "X" for "Extended Scheme" *)

  (**************** possible referents for a name ****************)

  datatype referent = LOCAL | PRIMITIVE of Primitive.primitive | OTHER_GLOBAL

  type name = string
  type environment = name list  (* list of local names *)
  val referent : name * environment -> referent
    = fn (x, locals) =>
         if List.exists (fn y => x = y) locals then
             LOCAL
         else
             case P.find x
               of SOME p => PRIMITIVE p
                | NONE => OTHER_GLOBAL

  (***************** machinery for disambiguating primtive functions *************)

  fun etaExpand p = (* used when a primitive occurs in a value position *)
    let val args = ["x", "y", "z", "w"]
        val formals = List.take (args, P.arity p)
    in  X.LAMBDA (formals, X.PRIMCALL (p, map X.LOCAL formals))
    end
  val _ = etaExpand : P.primitive -> X.exp

  (***************** translation of literals *************)

  fun value (S.PAIR (v1, v2)) = X.PRIMCALL (Primitive.cons, [value v1, value v2])
    | value (S.INT n)         = X.LITERAL (X.INT n)
    | value (S.REAL n)        = X.LITERAL (X.REAL n)
    | value (S.BOOLV b)       = X.LITERAL (X.BOOLV b)
    | value (S.EMPTYLIST)     = X.LITERAL X.EMPTYLIST
    | value (S.SYM x)         = X.LITERAL (X.SYM x)

  (***************** disambiguate expressions ******************)

  exception LeftAsExercise of string

  fun fst (x, y) = x

  val rec exp' : S.exp * environment -> X.exp = 
    fn (e, locals) =>
    let fun exp (S.LITERAL v)           = value v
          | exp (S.VAR x)               =
              raise LeftAsExercise "disambiguate VAR"
          | exp (S.SET (x, e))          = 
              raise LeftAsExercise "disambiguate SET"
          | exp (S.IFX (e1, e2, e3))    = X.IFX (exp e1, exp e2, exp e3)
          | exp (S.WHILEX (e1, e2))     = X.WHILEX (exp e1, exp e2)
          | exp (S.BEGIN es)            = X.BEGIN (map exp es)
          | exp (S.APPLY (e, es)) = 
              raise LeftAsExercise "disambiguate APPLY"
          | exp (S.LETX (S.LET, bindings, e)) =
              let val bs = map (fn (x, e) => (x, exp e)) bindings
                  val e = exp' (e, map fst bindings @ locals)
              in  X.LETX (X.LET, bs, e)
              end
          | exp (S.LETX (S.LETREC, bindings, e)) =
              let val locals = map fst bindings @ locals
                  val bs = map (fn (x, e) => (x, exp' (e, locals))) bindings
                  val e = exp' (e, locals)
              in  X.LETX (X.LETREC, bs, e)
              end
          | exp (S.LAMBDA (xs, e)) = X.LAMBDA (xs, exp' (e, xs @ locals))
    in  
        exp e
    end

  val expString = WppScheme.expString  (* prettyprinter for expressions *)

  fun def (S.DEFINE (f, (xs, e)))       = X.DEFINE (f, (xs, exp' (e, xs)))
    | def (S.VAL (f, S.LAMBDA (xs, e))) = X.DEFINE (f, (xs, exp' (e, xs)))
    | def (S.VAL (x, e)) = X.VAL (x, exp' (e, []))
    | def (S.EXP e) = X.EXP (exp' (e, []))
    | def (S.CHECK_EXPECT (e, e')) =
        X.CHECK_EXPECT (expString e, exp' (e, []), expString e', exp' (e', []))
    | def (S.CHECK_ASSERT e) =
        X.CHECK_ASSERT (expString e, exp' (e, []))

  val disambiguate = def

  (***************** re-embedding into ambiguous code *************)

  local
    (* every X literal is an S literal *)
    fun devalue (X.SYM x)     = S.SYM x
      | devalue (X.INT n)     = S.INT n
      | devalue (X.REAL n)     = S.REAL n
      | devalue (X.BOOLV b)   = S.BOOLV b
      | devalue (X.EMPTYLIST) = S.EMPTYLIST

    (* a `cons` of literals is also an S literal *)
    val rec maybeValue : X.exp -> S.value option 
      = fn (X.LITERAL v) => SOME (devalue v)
         | (X.PRIMCALL (opr, [e1, e2])) =>
             (case (P.name opr, maybeValue e1, maybeValue e2)
                of ("cons", SOME y, SOME ys) => SOME (S.PAIR (y, ys))
                 | _ => NONE)
         | _ => NONE


    (* To re-ambiguate:
         - An expression might be a literal value (`maybeValue`).
         - Both kinds of call are `APPLY`.
         - Both kinds of variable are `VAR`.
         - Both kinds of assignment are `SET`.
         - Everything else maps one to one.
     *)

    fun exp e = (case maybeValue e
                   of SOME v => S.LITERAL v
                    | NONE => exp' e)
    and exp' (X.FUNCALL (f, args))      = S.APPLY (exp f, map exp args)
      | exp' (X.PRIMCALL (p, es))       = S.APPLY (S.VAR (P.name p), map exp es)
      | exp' (X.LOCAL x)                = S.VAR x
      | exp' (X.GLOBAL x)               = S.VAR x
      | exp' (X.SETLOCAL (x, e))        = S.SET (x, exp e)
      | exp' (X.SETGLOBAL (x, e))       = S.SET (x, exp e)
      | exp' (X.LITERAL v)              = S.LITERAL (devalue v)
      | exp' (X.IFX (e1, e2, e3))       = S.IFX (exp e1, exp e2, exp e3)
      | exp' (X.LETX (X.LET, bs, e))    = S.LETX (S.LET, map binding bs, exp e)
      | exp' (X.LETX (X.LETREC, bs, e)) = S.LETX (S.LETREC, map binding bs, exp e)
      | exp' (X.BEGIN es)               = S.BEGIN (map exp es)
      | exp' (X.WHILEX (c, body))       = S.WHILEX (exp c, exp body)
      | exp' (X.LAMBDA (xs, e))         = S.LAMBDA (xs, exp e)
    and binding (x, e) = (x, exp e)

    fun def (X.EXP e)               = S.EXP (exp e)
      | def (X.VAL (x, e))          = S.VAL (x, exp e)
      | def (X.DEFINE (f, (xs, e))) = S.DEFINE (f, (xs, exp e))
      | def (X.CHECK_EXPECT (_, e, _, e')) = S.CHECK_EXPECT (exp e, exp e')
      | def (X.CHECK_ASSERT (_, e))        = S.CHECK_ASSERT (exp e)

  in

    val ambiguate = def
    val ambiguateExp = exp

  end

end
