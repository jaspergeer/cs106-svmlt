structure Mutability :> sig
  val detect     : UnambiguousVScheme.def -> UnambiguousVScheme.def Error.error
  val moveToHeap :  UnambiguousVScheme.def -> UnambiguousVScheme.def 
end
  =
struct
  structure X = UnambiguousVScheme


  fun snd (x, e) = e

  fun hasLambda (X.FUNCALL (f, args))      = anyLambda (f :: args)
    | hasLambda (X.PRIMCALL (p, es))       = anyLambda es
    | hasLambda (X.LOCAL x)                = false
    | hasLambda (X.GLOBAL x)               = false
    | hasLambda (X.SETLOCAL (x, e))        = hasLambda e
    | hasLambda (X.SETGLOBAL (x, e))       = hasLambda e
    | hasLambda (X.LITERAL v)              = false
    | hasLambda (X.IFX (e1, e2, e3))       = anyLambda [e1, e2, e3]
    | hasLambda (X.LETX (_, bs, e))        = hasLambda e orelse anyLambda (map snd bs)
    | hasLambda (X.BEGIN es)               = anyLambda es
    | hasLambda (X.WHILEX (c, body))       = hasLambda c orelse hasLambda body
    | hasLambda (X.LAMBDA (xs, e))         = true
  and anyLambda es = List.exists hasLambda es

  fun setsLocal (X.FUNCALL (f, args))      = anySet (f :: args)
    | setsLocal (X.PRIMCALL (p, es))       = anySet es
    | setsLocal (X.LOCAL x)                = false
    | setsLocal (X.GLOBAL x)               = false
    | setsLocal (X.SETLOCAL (x, e))        = true
    | setsLocal (X.SETGLOBAL (x, e))       = setsLocal e
    | setsLocal (X.LITERAL v)              = false
    | setsLocal (X.IFX (e1, e2, e3))       = anySet [e1, e2, e3]
    | setsLocal (X.LETX (_, bs, e))        = setsLocal e orelse anySet (map snd bs)
    | setsLocal (X.BEGIN es)               = anySet es
    | setsLocal (X.WHILEX (c, body))       = setsLocal c orelse setsLocal body
    | setsLocal (X.LAMBDA (xs, e))         = setsLocal e
  and anySet es = List.exists setsLocal es

  fun badExp e = hasLambda e andalso setsLocal e

  fun badDef (X.EXP e)               = badExp e
    | badDef (X.VAL (x, e))          = badExp e
    | badDef (X.DEFINE (f, (xs, e))) = badExp e
    | badDef (X.CHECK_EXPECT (_, e, _, e')) = badExp e orelse badExp e'
    | badDef (X.CHECK_ASSERT (_, e))        = badExp e

  fun detect d =
      if badDef d then
          Error.ERROR "a definition with `lambda` sets a local variable"
      else
          Error.OK d

  val detect = Error.OK

  fun moveToHeap _ = Impossible.exercise "migrate mutable, captured variables to heap"

end
