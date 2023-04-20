(* Parser for VScheme *)

(* You'll need to use the signature, 
    but don't need to know how it's implemented *)

functor VSchemeParsersFun (val useVcons : bool) :> sig
  val exp  : Sx.sx -> VScheme.exp Error.error
  val defs : Sx.sx -> VScheme.def list Error.error
end
  =
struct
  structure S  = VScheme

  fun whatAreTokens ts = "[" ^ String.concatWith "\194\183" (map SxUtil.whatis ts) ^ "]"
  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map SxUtil.show ts) ^ "]"

  structure P = MkListProducer (val species = "S-expression parser"
                                type input = Sx.sx
                                val show = showTokens
                               )

  (* wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = P.<*>
  infixr 4 <$>     val op <$> = P.<$>
  infix 3 <~>      val op <~> = P.<~>
  infix 1 <|>      val op <|> = P.<|>
  infix 3 >>       val op >> = P.>>
  
  val succeed = P.succeed
  val curry = P.curry
  val curry3 = P.curry3
  val id = P.id
  val fst = P.fst
  val snd = P.snd
  val many = P.many
  val many1 = P.many1
  val sat = P.sat
  val one = P.one
  val notFollowedBy = P.notFollowedBy
  val eos = P.eos

  infix 4 <$
  fun v <$ p = succeed v <~> p


  fun id x = x

  fun letx k bs e = S.LETX (k, bs, e)

  fun letstar [] e            = e
    | letstar ((x, e')::bs) e = letx S.LET [(x, e')] (letstar bs e)

  fun sexp (Sx.SYM s) = S.SYM s
    | sexp (Sx.INT n) = S.INT n
    | sexp (Sx.BOOL b) = S.BOOLV b
    | sexp (Sx.REAL y) = S.REAL y
    | sexp (Sx.LIST []) = S.EMPTYLIST
    | sexp (Sx.LIST (v::vs)) = S.PAIR (sexp v, sexp (Sx.LIST vs))

  val rwords =
    ["set", "if", "while", "begin", "let", "let*", "letrec", "lambda", "quote",
     "val", "define",
     (* next are from uML, for module 12 *)
     "case", "data", "implicit-data", "check-principal-type*", "record", "check-type", ":"
    ]

  fun reserved s = List.exists (P.eq s) rwords

  val int       = P.maybe (fn (Sx.INT   n)    => SOME n  | _ => NONE) one
  val sym       = P.maybe (fn (Sx.SYM s)      => SOME s  | _ => NONE) one
  val list      = P.maybe (fn (Sx.LIST sxs)    => SOME sxs  | _ => NONE) one
  val bool      = P.maybe (fn (Sx.BOOL b)    => SOME b  | _ => NONE) one
  val sxreal    = P.maybe (fn (Sx.REAL x)    => SOME x  | _ => NONE) one

  fun badInput msg tokens =
    SOME (Error.ERROR (msg ^ ": input " ^ showTokens tokens ^
                       " --> " ^ whatAreTokens tokens ), [])

  fun bad msg = P.ofFunction (badInput msg)

  fun kw word = P.sat (P.eq word) sym  (* keyword *)
  val namelike = sym

  fun isVcon x =
    useVcons andalso
    let val lastPart = List.last (String.fields (curry op = #".") x)
        val firstAfterdot = String.sub (lastPart, 0) handle Subscript => #" "
    in  x = "cons" orelse x = "'()" orelse
        Char.isUpper firstAfterdot orelse firstAfterdot = #"#" orelse
        String.isPrefix "make-" x
    end
  fun isVvar x = x <> "->" andalso not (isVcon x)
  val _ = if useVcons andalso isVvar "CAPTURED-IN" then Impossible.impossible "vvar"
          else ()
  val vvar = P.sat isVvar namelike
  val name =  P.sat (not o reserved) vvar
          <|> (P.check ((fn s => Error.ERROR ("reserved word \"" ^ s ^ "\" used as name")) <$> vvar))

  fun parseSucceeds (SOME (Error.OK a, [])) = SOME (Error.OK a)
    | parseSucceeds (SOME (Error.ERROR msg, _)) = SOME (Error.ERROR msg)
    | parseSucceeds _ = NONE

  fun oflist parser = P.check (P.maybe parseSucceeds (P.asFunction parser <$> list))

  type 'a parser = 'a P.producer

  val _ = oflist : 'a parser -> 'a parser

  fun expected what = bad ("expected " ^ what)

  fun bracket word parser =
    oflist (kw word >> (parser <~> P.eos <|> P.pzero))
(* P.perror ("Bad " ^ word ^ " form"))) *)

  fun exactList what parser =
    oflist (parser <~> P.eos <|> expected what)

  val vcon = 
    if useVcons then
      let fun isEmptyList S.EMPTYLIST = true
            | isEmptyList _ = false
          val boolname = (fn p => if p then "#t" else "#f")
      in     boolname <$> bool
         <|> sat isVcon namelike
         <|> "'()" <$ bracket "quote" (sat (isEmptyList o sexp) one)
      end
    else
      P.pzero


  val pattern = P.fix (fn pattern =>
                Pattern.WILDCARD    <$ sat (curry op = "_") vvar
      <|>       Pattern.VAR        <$> vvar
      <|> curry Pattern.APPLY      <$> vcon <*> succeed []
      <|> exactList "(C x1 x2 ...) in pattern"
                  (curry Pattern.APPLY <$> vcon <*> many pattern)
       )
      <|> expected "pattern"

  fun freeIn exp y =
    let fun member y [] = false
          | member y (z::zs) = y = z orelse member y zs
        fun has_y (S.LITERAL _) = false
          | has_y (S.VAR x) = x = y
          | has_y (S.SET (x, e)) = x = y orelse has_y e
          | has_y (S.IFX (e1, e2, e3))  = List.exists has_y [e1, e2, e3]
          | has_y (S.WHILEX (e1, e2))   = List.exists has_y [e1, e2]
          | has_y (S.BEGIN es)          = List.exists has_y es
          | has_y (S.APPLY (e, es))     = List.exists has_y (e::es)
          | has_y (S.LETX (S.LET, bs, e)) = List.exists rhs_has_y bs orelse
                                        (not (member y (map fst bs))
                                        andalso has_y e)
          | has_y (S.LETX (S.LETREC, bs, e)) =
              not (member y (map fst bs)) andalso has_y e
          | has_y (S.LAMBDA (xs, e)) = not (member y xs) andalso has_y e
          | has_y (S.VCON _) = false
          | has_y (S.CASE (e, choices)) =
              has_y e orelse List.exists choice_has_y choices
          | has_y (S.COND qas) = List.exists (fn (q, a) => has_y q orelse has_y a) qas
        and choice_has_y (pat, exp) =
              not (binds_y pat) andalso has_y exp
        and binds_y (Pattern.VAR x) = x = y
          | binds_y (Pattern.WILDCARD) = false
          | binds_y (Pattern.INT _) = false
          | binds_y (Pattern.APPLY (_, pats)) = List.exists binds_y pats
        and rhs_has_y (_, e) = has_y e
    in  has_y exp
    end

  fun fresh e =
    let fun try n = let val x = "x" ^ Int.toString n
                    in  if freeIn e x then try (n + 1) else x
                    end
    in  try 1
    end

  fun andSugar [] = S.LITERAL (S.BOOLV true)
    | andSugar [e] = e
    | andSugar (e::es) = S.IFX (e, andSugar es, S.LITERAL (S.BOOLV false))

  fun orSugar [] = S.LITERAL (S.BOOLV false)
    | orSugar [e] = e
    | orSugar (e1::es) =
        let val e2 = orSugar es
            val x = fresh e2
        in  S.LETX (S.LET, [(x, e1)], S.IFX (S.VAR x, S.VAR x, e2))
        end

  fun realExp x = (* materialize a real number, using crippled Moscow ML Real *)
    let val n    = Real.trunc x
        val frac = x - real n  (* x = n + frac *)
        val denominator = 1000000
        val numerator = Real.round (frac * real denominator)
        fun prim p args = S.APPLY(S.VAR p, args)
        fun lit n = S.LITERAL (S.INT n)
    in  prim "+" [lit n, prim "/" [lit numerator, lit denominator]]
    end

  val realExp = S.LITERAL o S.REAL

  fun name_not_pat what =
   name <|> pattern >> P.perror ("this parser doesn't support patterns in `" ^ what ^ "` bindings")

  val exp = P.fix (fn exp =>
    let fun pair x y = (x, y)
        val binding  = oflist (pair <$> name_not_pat "let" <*> exp)
        val bindings = oflist (many binding)
        fun asLambda (e as S.LAMBDA _) = Error.OK e
          | asLambda e = Error.ERROR "a letrec form may bind only `lambda` expressions"
        val lambda = P.check (asLambda <$> exp)
        val lbindings = oflist (many (oflist (pair <$> name_not_pat "letrec" <*> lambda)))
        val quoted = S.LITERAL
        fun quoted sx =
          case (useVcons, sx)
            of (true, S.EMPTYLIST) => S.VCON "'()"
             | _ => S.LITERAL sx
    in     bracket "set"       (curry  S.SET <$> name <*> exp)
       <|> bracket "if"        (curry3 S.IFX <$> exp <*> exp <*> exp)
       <|> bracket "while"     (curry  S.WHILEX <$> exp <*> exp)
       <|> bracket "begin"     (       S.BEGIN <$> many exp)
       <|> bracket "break"     (Impossible.exercise <$> succeed "AST for break")
       <|> bracket "continue"  (Impossible.exercise <$> succeed "AST for continue")
       <|> bracket "let"       (letx S.LET    <$> bindings <*> exp)
       <|> bracket "let*"      (letstar       <$> bindings <*> exp)
       <|> bracket "letrec"    (letx S.LETREC <$> lbindings <*> exp)
       <|> bracket "quote"     (      (quoted o sexp) <$> one)
       <|> bracket "lambda"    (curry S.LAMBDA <$> oflist (many name) <*> exp) 
       <|> bracket "||"        (orSugar <$> many exp) 
       <|> bracket "&&"        (andSugar <$> many exp) 
       <|> bracket "case" (curry S.CASE <$> exp <*> many (exactList "[pattern exp]" (pair <$> pattern <*> exp)))
       <|> oflist eos >> P.perror "empty list as Scheme expression"
       <|> realExp <$> sxreal
       <|> S.LITERAL <$> (    kw "#t" >> P.succeed (S.BOOLV true)
                          <|> kw "#f" >> P.succeed (S.BOOLV false)
                          <|> S.BOOLV <$> bool
                          <|> S.INT <$> int
                         )
       <|> S.VCON <$> vcon
       <|> S.VAR <$> name
       <|> oflist (curry S.APPLY <$> exp <*> many exp) 
                   
    end)

  fun unimp s _ = Impossible.unimp s

  fun define f xs e = S.DEFINE (f, (xs, e))

  fun single x = [x]

  fun l1 arityx K = arityx (single o K)

  local
    fun nullp x = S.APPLY(S.VAR "null?", [x])
    fun pairp x = S.APPLY(S.VAR "pair?", [x])
    val cons = fn (x, xs) => VSchemeUtils.cons x xs
    structure P = Pattern

        fun desugarRecord _ recname fieldnames =
              recordPredicate recname fieldnames ::
              recordAccessors recname 0 fieldnames @
              recordMutators recname 0 fieldnames
        and recordConstructor recname fieldnames = 
              let val con = "make-" ^ recname
                  val formals = map (fn s => "the-" ^ s) fieldnames
                  val body = S.APPLY (S.VCON con, map S.VAR formals)
              in  S.DEFINE (con, (formals, body))
              end
        and recordPredicate recname fieldnames =
              let val con = "make-" ^ recname
                  val choices =
                      [ (P.APPLY (con, map (fn _ => P.WILDCARD) fieldnames),
                         S.LITERAL (S.BOOLV true))
                      , (P.WILDCARD, S.LITERAL (S.BOOLV false))
                      ]
                  val predname = recname ^ "?"
                  val r = S.VAR "r"
                  val formals = ["r"]
              in  S.DEFINE (predname, (formals, S.CASE (r, choices)))
              end
        and recordAccessors recname n fields =
              let val con = "make-" ^ recname
                  val pat = P.APPLY (con, map P.VAR fields)
                  fun accessor field =
                    let val accname = recname ^ "-" ^ field
                        val choices =
                            [ (pat, S.VAR field)
                            , (P.WILDCARD
                              , S.APPLY (S.VAR "error", [S.LITERAL (S.SYM ("value passed to " ^ accname ^ " is not a " ^ recname ^ " record"))])
                              )
                            ]
                        val r = S.VAR "r"
                        val formals = ["r"]
                    in  S.DEFINE (accname, (formals, S.CASE (r, choices)))
                    end
              in  map accessor fields
              end

          and recordMutators recname n _ = []
(*
            | recordMutators recname n (field::fields) =
                let val predname = recname ^ "?"
                    val mutname = "set-" ^ recname ^ "-" ^ field ^ "!"
                    val formals = ["r", "v"]
                    val setfield = VSchemeUtils.setcar (cdrs (n+1, S.VAR "r")) (S.VAR "v")
                    val body = S.IFX ( S.APPLY (S.VAR predname, [S.VAR "r"])
                                   , setfield
                                   , error (S.SYM (concat ["value-passed-to"
                                                 , mutname
                                                 , "-is-not-a-"
                                                 , recname
                                                 ])))
                in  S.DEFINE (mutname, (formals, body)) ::
                    recordMutators recname (n+1) fields
                end
*)
        and and_also (p, q) = S.IFX (p, q, S.LITERAL (S.BOOLV false))
        and cdrs (0, xs) = xs
          | cdrs (n, xs) = VSchemeUtils.cdr (cdrs (n-1, xs))

        and list [] = S.LITERAL S.EMPTYLIST
          | list (v::vs) = cons (S.LITERAL v, list vs)
        and error x = S.APPLY (S.VAR "error", [S.LITERAL x])

        and varlist [] = S.LITERAL S.EMPTYLIST
          | varlist (x::xs) = cons (S.VAR x, varlist xs)
  in 
    val desugarRecord = desugarRecord
  end


  fun single x = [x]

  val funname
      = (P.check ((fn f => Error.ERROR ("cannot use value constructor " ^ f ^ " as function name")) <$> vcon))
      <|> name

  val typedefIgnored =
    many one >> succeed (S.EXP (S.LITERAL (S.SYM "type definition ignored")))

  val field = name <|> exactList "typed record field" (name <~> kw ":" <~> one)

  val optionalTyvars = SOME <$> oflist (many one) <|> succeed NONE

  val def =
    single <$>
           (    bracket "val"    (curry S.VAL <$> name_not_pat "val" <*> exp)
            <|> bracket "define" (define <$> funname <*> oflist (many name) <*> exp)
            <|> bracket "check-expect"  (curry  S.CHECK_EXPECT <$> exp <*> exp)
            <|> bracket "check-assert"  (       S.CHECK_ASSERT <$> exp)
            <|> bracket "check-error"   (unimp "check-error" <$> exp)
            <|> bracket "data"          typedefIgnored
            <|> bracket "implicit-data" typedefIgnored
            <|> bracket "check-principal-type*" typedefIgnored
            <|> bracket "check-type" typedefIgnored
           )
    <|> bracket "record" (desugarRecord <$> optionalTyvars <*> name <*> oflist (many field))
    <|> single <$> S.EXP <$> exp

  fun transform parser sx = P.produce (parser <~> P.eos) [sx]

  val exp = transform exp
  val defs = transform def
end

structure VSchemeParsers = VSchemeParsersFun (val useVcons = false)
structure EschemeParsers = VSchemeParsersFun (val useVcons = true)
