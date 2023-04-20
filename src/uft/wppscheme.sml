(* Part of the Pretty Printer *)

(* You can ignore this *)

structure WppScheme :> sig
  val pp    : VScheme.def -> Wppx.doc
  val ppexp : VScheme.exp -> Wppx.doc
  val expString : VScheme.exp -> string  (* for use with check and expect *)
  val patString : Pattern.pat -> string
end
  =
struct
  structure S = VScheme
  structure P = Wppx

  val ++ = Wppx.^^
  infix 7 ++

  infixr 0 $
  fun f $ x = f x

  val nest = P.nest
  val te = P.text
  val cn = te " " ++ P.Line.connected
  val nl = P.Line.forced
  val on = te " " ++ P.Line.optional

  fun id x = x

  fun letkeyword S.LET     = "let"
    | letkeyword S.LETREC  = "letrec"

  fun value (S.SYM v)   = P.text v
    | value (S.INT n)   = P.int n
    | value (S.REAL n)  = P.real n
    | value (S.BOOLV b) = P.text (if b then "#t" else "#f")
    | value (S.EMPTYLIST)     = P.text "()"
    | value (S.PAIR (car, cdr))  = 
        P.group (P.text "(" ++ P.seq cn id (values (S.PAIR (car, cdr))) ++ P.text ")")
  and values (S.EMPTYLIST) = []
    | values (S.PAIR (car, cdr)) = value car :: values cdr
    | values v = [P.text ".", value v]

  val line = te "<line>"
  val linew = te "<linew>"
  val lines = te "<lines>"
  val linek = te "<linek>"

  val line = on
  val linew = on
  val lines = on
  val linek = on

  fun bracket docs = P.group (te "(" ++ P.concat docs ++ te ")")
  fun square  docs = P.group (te "[" ++ P.concat docs ++ te "]")
  fun wrap  docs = P.group (te "(" ++ P.seq cn id docs ++ te ")")
  fun wraps docs = P.group (te "[" ++ P.seq cn id docs ++ te "]")
  fun kw k docs = P.group (te "(" ++ te k ++ te " " ++ P.seq cn id docs ++ te ")")
  fun kwbreak k b docs = P.group (te "(" ++ te k ++ te " " ++ b ++ P.seq cn id docs ++ te ")")

  structure Pat = Pattern
  fun pat (Pat.INT k) = P.int k
    | pat (Pat.VAR x) = te x
    | pat (Pat.WILDCARD) = te "_"
    | pat (Pat.APPLY (vcon, [])) = te vcon
    | pat (Pat.APPLY (vcon, pats)) = nest 3 (wrap (te vcon :: map pat pats))

  fun exp e =
     let
         fun pplet thekw bs e =
           let val i = size thekw + 3
               fun binding (x, e) =
                 let val indent = Int.min (size x + 2, 6)
                 in  square [te x, P.group (P.nest indent (on ++ exp e))]
                 end
               val bindings = P.nest 0 (P.seq nl binding bs)
           in  nest i (te "(" ++ te thekw ++ te " " ++ wrap [bindings] ++
                          nest (2 - i) (cn ++ exp e ++ te ")"))
           end

         fun nestedBindings (prefix', S.LETX (S.LET, [(x, e')], e)) =
               nestedBindings ((x, e') :: prefix', e)
           | nestedBindings (prefix', e) = (rev prefix', e)

     in  case e
           of S.LITERAL (v as S.INT   _) => value v
            | S.LITERAL (v as S.REAL  _) => value v
            | S.LITERAL (v as S.BOOLV _) => value v
            | S.LITERAL v => te "'" ++ value v
            | S.VAR name => te name
            | S.SET (x, e) =>
                nest 3 (kw "set" [te x, exp e])
            | S.IFX (e1, e2, e3) =>
                nest 3 (kw "if" (map exp [e1, e2, e3]))
            | S.WHILEX (e1, e2) =>
                nest 3 (kw "while" (map exp [e1, e2]))
            | S.BEGIN es => 
                nest 3 $ bracket [te "begin", cn, P.seq cn exp es]
            | S.APPLY (S.VCON k, []) =>
                te k
            | S.APPLY (e, es) => 
                nest 3 (wrap (map exp (e::es)))
            | S.LETX (S.LET, bs as [(x, e')], e) =>
                let val (bs, e) = nestedBindings (bs, e)
                in  case bs
                      of [] => exp e
                       | [_] => pplet "let" bs e
                       | _ => pplet "let*" bs e
                end
            | S.LETX (lk, bs, e) => 
                let fun binding (x, e) = nest 3 (wraps [te x, exp e])
                    val bindings = P.seq cn binding bs
                in  nest 3 (kwbreak (letkeyword lk) on [wrap [bindings], exp e])
                end
            | S.LAMBDA (xs, body) =>
                nest 3 (kw "lambda" [wrap (map te xs), exp body])
            | S.COND qas =>
                let fun qa (q, a) = nest 6 (wraps [exp q, exp a])
                in  nest 3 (kw "cond" [on, P.seq cn qa qas])
                end
            | S.CASE (e, choices) => 
                let fun choice (p, e) = nest 6 (wraps [pat p, exp e])
                in  nest 3 (kw "case" [exp e, P.seq cn choice choices])
                end
            | S.VCON "'()" => te "'()"
            | S.VCON k => te ("'" ^ k)
     end
 

   fun def d =
     case d
       of S.VAL (x, e) => nest 3 (kw "val" [te x, exp e])
        | S.DEFINE (f, (xs, e)) =>
            nest 3 (kw "define" [te f, wrap (map te xs), exp e])
        | S.CHECK_EXPECT (e, S.LITERAL (S.BOOLV true)) =>
            nest 3 (kw "check-assert" [exp e])
        | S.CHECK_EXPECT (e, e') =>
            nest 3 (kw "check-expect" [exp e, exp e'])
        | S.CHECK_ASSERT e =>
            nest 3 (kw "check-assert" [exp e])
        | S.EXP e => exp e


  val pp = def
  val ppexp = exp

  fun stripFinalNewline s =
    case rev (explode s)
      of #"\n" :: cs => implode (rev cs)
       | _ => s
  
  val expString = stripFinalNewline o Wppx.toString 60 o ppexp
  val patString = stripFinalNewline o Wppx.toString 60 o pat

end

