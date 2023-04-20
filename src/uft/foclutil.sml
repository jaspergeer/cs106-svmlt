(* Embedding First-Order Scheme into Closed Scheme. This cannot fail. *)
(* Used to read ClosedScheme from a file (piggyback on the FO reader) *)

(* You don't need to look at this *)

structure FOCLUtil :> sig
  val embed : FirstOrderScheme.def -> ClosedScheme.def
end
  =
struct
  structure F = FirstOrderScheme
  structure C = ClosedScheme

  fun exp (F.LITERAL v) = C.LITERAL v
    | exp (F.LOCAL x)   = C.LOCAL x
    | exp (F.GLOBAL x)  = C.GLOBAL x
    | exp (F.IFX (e1, e2, e3)) = C.IFX (exp e1, exp e2, exp e3)
    | exp (F.PRIMCALL (p, es)) = C.PRIMCALL (p, map exp es)
    | exp (F.FUNCALL (e, es))  = C.FUNCALL (exp e, map exp es)
    | exp (F.LET     (bs, e))  = C.LET     (map binding bs, exp e)
    | exp (F.BEGIN es)         = C.BEGIN   (map exp es)
    | exp (F.SETLOCAL (x, e))  = C.SETLOCAL (x, exp e)
    | exp (F.SETGLOBAL (x, e)) = C.SETGLOBAL (x, exp e)
    | exp (F.WHILEX (c, body)) = C.WHILEX (exp c, exp body)
    | exp (F.CASE c)           = C.CASE (Case.map exp c)
    | exp (F.CONSTRUCTED c)    = C.CONSTRUCTED (Constructed.map exp c)
  and binding (x, e) = (x, exp e)

  fun def (F.VAL (x, e)) = C.VAL (x, exp e)
    | def (F.EXP e)      = C.EXP (exp e)
    | def (F.DEFINE (f, (xs, e)))         = C.DEFINE (f, (xs, exp e))
    | def (F.CHECK_EXPECT (s, e, s', e')) = C.CHECK_EXPECT (s, exp e, s', exp e')
    | def (F.CHECK_ASSERT (s, e))         = C.CHECK_ASSERT (s, exp e)

  val embed = def

end
