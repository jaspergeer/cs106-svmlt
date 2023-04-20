(* Embedding and projection between disambiguated VScheme and 
    First-Order Scheme. Note that projection can fail; embedding can not. *)

(* This code is so boring it will make your eyes bleed.  You already understand it. *)


structure FOUtil :> sig
  val project : UnambiguousVScheme.def -> FirstOrderScheme.def Error.error
  val embed : FirstOrderScheme.def -> VScheme.def
  val embedExp : FirstOrderScheme.exp -> VScheme.exp
end
  =
struct
  (* projection accepts everything except `lambda` and `letrec` *)

  structure X = UnambiguousVScheme
  structure F = FirstOrderScheme
  structure P  = Pattern
  structure LU = ListUtil
  structure SU = VSchemeUtils

  infix 3 <*>   val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>
  val succeed = Error.succeed
  val error = Error.ERROR
  val errorList = Error.list

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun pair x y = (x, y)

  val expString = WppScheme.expString o Disambiguate.ambiguateExp

  fun reject form = error ("Expression `" ^ form ^ "` isn't in first-order Scheme.")
 
  fun lambdaOf formals =
    String.concat ["(lambda (", String.concatWith " " formals, ") ...)"]

  fun whenSmall msg = (* keep message only if small *)
    if size msg < 60 then SOME msg else NONE


  local  (* projection *)
    fun exp (X.LITERAL v)        = succeed (F.LITERAL (KNProject.value v))
      | exp (X.LOCAL x)          = succeed (F.LOCAL x)
      | exp (X.GLOBAL x)         = succeed (F.GLOBAL x)
      | exp (X.SETLOCAL (x, e))  = curry F.SETLOCAL  x <$> exp e
      | exp (X.SETGLOBAL (x, e)) = curry F.SETGLOBAL x <$> exp e
      | exp (X.IFX (e1, e2, e3)) = curry3 F.IFX <$> exp e1 <*> exp e2 <*> exp e3
      | exp (X.WHILEX (e1, e2))  = curry F.WHILEX <$> exp e1 <*> exp e2
      | exp (X.BEGIN es)         = F.BEGIN <$> exps es
      | exp (X.FUNCALL (e, es))  = curry F.FUNCALL <$> exp e <*> exps es
      | exp (X.PRIMCALL (x, es)) = curry F.PRIMCALL x <$> exps es
      | exp (X.LETX (X.LET,     bs, body)) = curry F.LET     <$> bindings bs <*> exp body
      | exp (e as X.LETX (X.LETREC, _, _))  = reject "(letrec (...) ...)"
      | exp (e as X.LAMBDA (formals, _)) =
          reject (getOpt (whenSmall (expString e), lambdaOf formals))
      | exp (X.CASE c) = F.CASE <$> Case.liftError (Case.map exp c)
      | exp (X.CONSTRUCTED (con, es)) = curry F.CONSTRUCTED con <$> exps es
    and exps es = Error.list (map exp es)
    and bindings bs = Error.list (map (fn (x, e) => pair x <$> exp e) bs)


    fun def (X.VAL (x, e))          = curry F.VAL x <$> exp e
      | def (X.EXP e)               = F.EXP <$> exp e
      | def (X.DEFINE (x, (ns, e))) = curry F.DEFINE x <$> (pair ns <$> exp e)
      | def (X.CHECK_EXPECT (s, e, s', e')) =
          (fn e => fn e' => F.CHECK_EXPECT (s, e, s', e')) <$> exp e <*> exp e'
      | def (X.CHECK_ASSERT (s, e)) =
          (fn e => F.CHECK_ASSERT (s, e)) <$> exp e
  in
    val project = def
  end


  local (* embedding *)
    structure C = VScheme
    fun exp (F.LITERAL v) = C.LITERAL (KNEmbed.value v)
      | exp (F.LOCAL x)   = C.VAR x
      | exp (F.GLOBAL x)  = C.VAR x
      | exp (F.IFX (e1, e2, e3)) = C.IFX (exp e1, exp e2, exp e3)
      | exp (F.PRIMCALL (p, es)) = C.APPLY (C.VAR (Primitive.name p), map exp es)
      | exp (F.FUNCALL (e, es))  = C.APPLY (exp e, map exp es)
      | exp (F.LET     (bs, e))  = C.LETX (C.LET, map binding bs, exp e)
      | exp (F.BEGIN es)         = C.BEGIN   (map exp es)
      | exp (F.SETLOCAL (x, e))  = C.SET (x, exp e)
      | exp (F.SETGLOBAL (x, e)) = C.SET (x, exp e)
      | exp (F.WHILEX (c, body)) = C.WHILEX (exp c, exp body)
      | exp (F.CONSTRUCTED (con, es)) = SU.constructed con (map exp es)
      | exp (F.CASE c) = C.CASE (Case.map exp c)
    and binding (x, e) = (x, exp e)

    fun def (F.VAL (x, e)) = C.VAL (x, exp e)
      | def (F.EXP e)      = C.EXP (exp e)
      | def (F.DEFINE (f, (xs, e))) = C.DEFINE (f, (xs, exp e))
      | def (F.CHECK_EXPECT (s, e, s', e')) = C.CHECK_EXPECT (exp e, exp e')
      | def (F.CHECK_ASSERT (s, e)) = C.CHECK_ASSERT (exp e)

  in
    val embed = def
    val embedExp = exp
  end

end
