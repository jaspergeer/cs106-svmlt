(* mlscheme.sml S215a *)


(*****************************************************************)
(*                                                               *)
(*   \FOOTNOTESIZE SHARED: NAMES, ENVIRONMENTS, STRINGS, ERRORS, PRINTING, INTERACTION, STREAMS, \&\ INITIALIZATION *)
(*                                                               *)
(*****************************************************************)

(* \footnotesize shared: names, environments, strings, errors, printing, interaction, streams, \&\ initialization S69a *)
(* for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]] S95c *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* type declarations for consistency checking *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* support for names and environments 302a *)
type name = string
(* support for names and environments 302b *)
type 'a env = (name * 'a) list
(* support for names and environments 303a *)
val emptyEnv = []
(* support for names and environments 303b *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find (name, tail)
(* support for names and environments 304a *)
fun bind (name, v, rho) =
  (name, v) :: rho
(* support for names and environments 304b *)
exception BindListLength
fun bindList (x::vars, v::vals, rho) = bindList (vars, vals, bind (x, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength

fun mkEnv (xs, vs) = bindList (xs, vs, emptyEnv)
(* support for names and environments 304c *)
(* composition *)
infix 6 <+>
fun pairs <+> pairs' = pairs' @ pairs
(* type declarations for consistency checking *)
val _ = op emptyEnv : 'a env
(* type declarations for consistency checking *)
val _ = op find : name * 'a env -> 'a
(* type declarations for consistency checking *)
val _ = op bind : name * 'a * 'a env -> 'a env
(* type declarations for consistency checking *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* type declarations for consistency checking *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* support for names and environments S208d *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* type declarations for consistency checking *)
val _ = op duplicatename : name list -> name option
(* support for detecting and signaling errors detected at run time S208c *)
exception RuntimeError of string (* error message *)
(* support for detecting and signaling errors detected at run time S208e *)
fun errorIfDups (what, xs, context) =
  case duplicatename xs
    of NONE   => ()
     | SOME x => raise RuntimeError (what ^ " " ^ x ^ " appears twice in " ^
                                                                        context)
(* type declarations for consistency checking *)
val _ = op errorIfDups : string * name list * string -> unit
(* support for detecting and signaling errors detected at run time S208f *)
exception InternalError of string (* bug in the interpreter *)
(* list functions not provided by \sml's initial basis S73b *)
fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths

fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end
(* list functions not provided by \sml's initial basis S73c *)
val reverse = rev
(* list functions not provided by \sml's initial basis S74a *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
(* utility functions for string manipulation and printing S70a *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* utility functions for string manipulation and printing S70b *)
val xprinter = ref print
fun xprint   s = !xprinter s
fun xprintln s = (xprint s; xprint "\n")
(* utility functions for string manipulation and printing S70c *)
fun tryFinally f x post =
  (f x handle e => (post (); raise e)) before post ()

fun withXprinter xp f x =
  let val oxp = !xprinter
      val ()  = xprinter := xp
  in  tryFinally f x (fn () => xprinter := oxp)
  end
(* utility functions for string manipulation and printing S70d *)
fun bprinter () =
  let val buffer = ref []
      fun bprint s = buffer := s :: !buffer
      fun contents () = concat (rev (!buffer))
  in  (bprint, contents)
  end
(* utility functions for string manipulation and printing S70e *)
fun predefinedFunctionError s = eprintln ("while reading predefined functions, "
                                                                            ^ s)
(* utility functions for string manipulation and printing S70f *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
fun realString x =
 (if Real.== (x, real (Real.floor x)) then
    intString (Real.floor x)
  else
    String.map (fn #"~" => #"-" | c => c) (Real.fmt (StringCvt.FIX (SOME 2)) x)
 ) handle Overflow => Real.toString x

(* utility functions for string manipulation and printing S70g *)
fun plural what [x] = what
  | plural what _   = what ^ "s"

fun countString xs what =
  intString (length xs) ^ " " ^ plural what xs
(* utility functions for string manipulation and printing S71a *)
fun separate (zero, sep) = 
  (* list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")   (* list separated by spaces *)
val commaSep = separate ("", ", ")  (* list separated by commas *)
(* type declarations for consistency checking *)
val _ = op intString : int -> string
(* type declarations for consistency checking *)
val _ = op spaceSep :                    string list -> string
val _ = op commaSep :                    string list -> string
val _ = op separate : string * string -> string list -> string
(* utility functions for string manipulation and printing S71b *)
fun printUTF8 code =
  let val w = Word.fromInt code
      val (&, >>) = (Word.andb, Word.>>)
      infix 6 & >>
      val _ = if (w & 0wx1fffff) <> w then
                raise RuntimeError (intString code ^
                                    " does not represent a Unicode code point")
              else
                 ()
      val printbyte = xprint o str o chr o Word.toInt
      fun prefix byte byte' = Word.orb (byte, byte')
  in  if w > 0wxffff then
        app printbyte [ prefix 0wxf0  (w >> 0w18)
                      , prefix 0wx80 ((w >> 0w12) & 0wx3f)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w      ) & 0wx3f)
                      ]
      else if w > 0wx7ff then
        app printbyte [ prefix 0wxe0  (w >> 0w12)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else if w > 0wx7f then
        app printbyte [ prefix 0wxc0  (w >>  0w6)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else
        printbyte w
  end
(* utility functions for string manipulation and printing S71c *)
fun fnvHash s =
  let val offset_basis = 0wx011C9DC5 : Word.word  (* trim the high bit *)
      val fnv_prime    = 0w16777619  : Word.word
      fun update (c, hash) = Word.xorb (hash, Word.fromInt (ord c)) * fnv_prime
      fun int w = Word.toIntX w handle Overflow => Word.toInt (Word.andb (w,
                                                                     0wxffffff))
  in  int (foldl update offset_basis (explode s))
  end
(* type declarations for consistency checking *)
val _ = op fnvHash : string -> int
(* utility functions for string manipulation and printing S72a *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* support for representing errors as \ml\ values S75b *)
datatype 'a error = OK of 'a | ERROR of string
(* support for representing errors as \ml\ values S76a *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* type declarations for consistency checking *)
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(* type declarations for consistency checking *)
val _ = op optionList : 'a option list -> 'a list option
(* type declarations for consistency checking *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* support for representing errors as \ml\ values S76b *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* type declarations for consistency checking *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* support for representing errors as \ml\ values S76c *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* type declarations for consistency checking *)
val _ = op errorList : 'a error list -> 'a list error
(* type [[interactivity]] plus related functions and value S210a *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* type [[interactivity]] plus related functions and value S210b *)
datatype output_interactivity = PRINTING | NOT_PRINTING
(* type [[interactivity]] plus related functions and value S210c *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_PRINTING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun prints (_, PRINTING)     = true
  | prints (_, NOT_PRINTING) = false
(* type declarations for consistency checking *)
type interactivity = interactivity
val _ = op noninteractive : interactivity
val _ = op prompts : interactivity -> bool
val _ = op prints  : interactivity -> bool
(* simple implementations of set operations S72b *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs
(* type declarations for consistency checking *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* collections with mapping and combining functions S72c *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* type declarations for consistency checking *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* collections with mapping and combining functions S73a *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* type declarations for consistency checking *)
val _ = op joinC   : 'a collection collection -> 'a collection
val _ = op mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
val _ = op filterC : ('a -> bool)    -> ('a collection -> 'a collection)
val _ = op mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
(* suspensions S81a *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(* type declarations for consistency checking *)
type 'a susp = 'a susp
(* suspensions S81b *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* type declarations for consistency checking *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
(* streams S82a *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* streams S82b *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* streams S82c *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* type declarations for consistency checking *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* type declarations for consistency checking *)
val _ = op streamOfList : 'a list -> 'a stream
(* streams S82d *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* streams S82e *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* type declarations for consistency checking *)
val _ = op listOfStream : 'a stream -> 'a list
(* type declarations for consistency checking *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* streams S83a *)
fun streamOfEffects action =
  delayedStream (fn () => case action () of NONE   => EOS
                                          | SOME a => a ::: streamOfEffects
                                                                         action)
(* type declarations for consistency checking *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* streams S83b *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* type declarations for consistency checking *)
type line = line
val _ = op filelines : TextIO.instream -> line stream
(* streams S83c *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* type declarations for consistency checking *)
val _ = op streamRepeat : 'a -> 'a stream
(* streams S83d *)
fun streamOfUnfold next state =
  delayedStream (fn () => case next state
                            of NONE => EOS
                             | SOME (a, state') => a ::: streamOfUnfold next
                                                                         state')
(* type declarations for consistency checking *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* streams S83e *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* type declarations for consistency checking *)
val _ = op naturals : int stream
(* streams S84a *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* streams S84b *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* type declarations for consistency checking *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* type declarations for consistency checking *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* streams S84c *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* streams S84d *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* type declarations for consistency checking *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* streams S84e *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* type declarations for consistency checking *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
(* streams S85a *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* streams S85b *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* type declarations for consistency checking *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* type declarations for consistency checking *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* streams S85c *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* streams S85d *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* type declarations for consistency checking *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* streams S85e *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(* type declarations for consistency checking *)
val _ = op streamTake : int * 'a stream -> 'a list
(* streams S86a *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* type declarations for consistency checking *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* stream transformers and their combinators S93a *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* type declarations for consistency checking *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* stream transformers and their combinators S93b *)
fun pure y = fn xs => SOME (OK y, xs)
(* type declarations for consistency checking *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* stream transformers and their combinators S95a *)
infix 3 <*>
fun tx_f <*> tx_b =
  fn xs => case tx_f xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK f, xs) =>
                  case tx_b xs
                    of NONE => NONE
                     | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
                     | SOME (OK y, xs) => SOME (OK (f y), xs)
(* type declarations for consistency checking *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S95b *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* type declarations for consistency checking *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S95d *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* type declarations for consistency checking *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S95e *)
fun pzero _ = NONE
(* stream transformers and their combinators S96a *)
fun anyParser ts = 
  foldr op <|> pzero ts
(* type declarations for consistency checking *)
val _ = op pzero : ('a, 'b) xformer
(* type declarations for consistency checking *)
val _ = op anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer
(* stream transformers and their combinators S96b *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* type declarations for consistency checking *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S96c *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* type declarations for consistency checking *)
val _ = op one : ('a, 'a) xformer
(* stream transformers and their combinators S97a *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op eos : ('a, unit) xformer
(* stream transformers and their combinators S97b *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* type declarations for consistency checking *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* stream transformers and their combinators S97c *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* type declarations for consistency checking *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S98a *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* type declarations for consistency checking *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S98b *)
fun eqx y = 
  sat (fn y' => y = y') 
(* type declarations for consistency checking *)
val _ = op eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
(* stream transformers and their combinators S98c *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* type declarations for consistency checking *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S98d *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* type declarations for consistency checking *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S98e *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* stream transformers and their combinators S99a *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* type declarations for consistency checking *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators S99b *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* type declarations for consistency checking *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators S99c *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* type declarations for consistency checking *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* stream transformers and their combinators S100a *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* type declarations for consistency checking *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)             * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
(* support for source-code locations and located streams S86c *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
(* support for source-code locations and located streams S86d *)
datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
val toplevel_error_format = ref WITH_LOCATIONS
(* support for source-code locations and located streams S86e *)
fun synerrormsg (source, line) strings =
  if !toplevel_error_format = WITHOUT_LOCATIONS andalso source =
                                                                "standard input"
  then
    concat ("syntax error: " :: strings)
  else    
    concat ("syntax error in " :: srclocString (source, line) :: ": " :: strings
                                                                               )

(* support for source-code locations and located streams S86f *)
exception Located of srcloc * exn
(* support for source-code locations and located streams S87a *)
type 'a located = srcloc * 'a
(* type declarations for consistency checking *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* type declarations for consistency checking *)
type 'a located = 'a located
(* support for source-code locations and located streams S87b *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* more handlers for [[atLoc]] S87d *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
(* type declarations for consistency checking *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* support for source-code locations and located streams S87c *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* type declarations for consistency checking *)
val _ = op located : ('a -> 'b) -> ('a located -> 'b)
val _ = op leftLocated : ('a * 'b -> 'c) -> ('a located * 'b -> 'c)
(* support for source-code locations and located streams S87e *)
fun fillComplaintTemplate (s, maybeLoc) =
  let val string_to_fill = " <at loc>"
      val (prefix, atloc) = Substring.position string_to_fill (Substring.full s)
      val suffix = Substring.triml (size string_to_fill) atloc
      val splice_in =
        Substring.full (case maybeLoc
                          of NONE => ""
                           | SOME (loc as (file, line)) =>
                               if      !toplevel_error_format =
                                                               WITHOUT_LOCATIONS
                               andalso file = "standard input"
                               then
                                 ""
                               else
                                 " in " ^ srclocString loc)
  in  if Substring.size atloc = 0 then (* <at loc> is not present *)
        s
      else
        Substring.concat [prefix, splice_in, suffix]
  end
fun fillAtLoc (s, loc) = fillComplaintTemplate (s, SOME loc)
fun stripAtLoc s = fillComplaintTemplate (s, NONE)
(* type declarations for consistency checking *)
val _ = op fillComplaintTemplate : string * srcloc option -> string
(* support for source-code locations and located streams S88a *)
fun errorAt msg loc = 
  ERROR (synerrormsg loc [msg])
(* support for source-code locations and located streams S88b *)
fun locatedStream (streamname, inputs) =
  let val locations = streamZip (streamRepeat streamname, streamDrop (1,
                                                                      naturals))
  in  streamZip (locations, inputs)
  end
(* type declarations for consistency checking *)
val _ = op errorAt : string -> srcloc -> 'a error
(* type declarations for consistency checking *)
val _ = op locatedStream : string * line stream -> line located stream
(* streams that track line boundaries S104a *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(* streams that track line boundaries S104b *)
local 
  fun asEol (EOL n) = SOME n
    | asEol (INLINE _) = NONE
  fun asInline (INLINE x) = SOME x
    | asInline (EOL _)    = NONE
in
  fun eol    xs = (asEol    <$>? one) xs
  fun inline xs = (asInline <$>? many eol *> one) xs
  fun srcloc xs = rewind (fst <$> inline) xs
end
(* type declarations for consistency checking *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* type declarations for consistency checking *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* support for lexical analysis S100b *)
type 'a lexer = (char, 'a) xformer
(* type declarations for consistency checking *)
type 'a lexer = 'a lexer
(* support for lexical analysis S100c *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* type declarations for consistency checking *)
val _ = op isDelim : char -> bool
(* support for lexical analysis S100d *)
val whitespace = many (sat Char.isSpace one)
(* type declarations for consistency checking *)
val _ = op whitespace : char list lexer
(* support for lexical analysis S102a *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*> many1 (sat Char.isDigit one)
                                                                              <*
  notFollowedBy (sat (not o isDelim) one)

fun realChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*>
      (curry (op @) <$> many1 (sat Char.isDigit one) <*>
         (curry (op ::) <$> eqx #"." one <*> many (sat Char.isDigit one)))
                                                                              <*
  notFollowedBy (sat (not o isDelim) one)


(* type declarations for consistency checking *)
val _ = op intChars : (char -> bool) -> char list lexer
(* support for lexical analysis S102b *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow => ERROR
                        "this interpreter can't read arbitrarily large integers"

fun realFromChars (#"-" :: cs) = 
      realFromChars cs >>=+ Real.~
  | realFromChars cs =
      (OK o valOf o Real.fromString o implode) cs

(* type declarations for consistency checking *)
val _ = op intFromChars : char list -> int error
(* support for lexical analysis S102c *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim

fun realToken isDelim =
  realFromChars <$>! realChars isDelim

(* type declarations for consistency checking *)
val _ = op intToken : (char -> bool) -> int lexer
val _ = op realToken : (char -> bool) -> real lexer
(* support for lexical analysis S102d *)
datatype bracket_shape = ROUND | SQUARE | CURLY

fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"
(* support for lexical analysis S103 *)
datatype 'a plus_brackets
  = LEFT  of bracket_shape
  | RIGHT of bracket_shape
  | PRETOKEN of 'a

fun bracketLexer pretoken
  =  LEFT  ROUND  <$ eqx #"(" one
 <|> LEFT  SQUARE <$ eqx #"[" one
 <|> LEFT  CURLY  <$ eqx #"{" one
 <|> RIGHT ROUND  <$ eqx #")" one
 <|> RIGHT SQUARE <$ eqx #"]" one
 <|> RIGHT CURLY  <$ eqx #"}" one
 <|> PRETOKEN <$> pretoken

fun plusBracketsString _   (LEFT shape)  = leftString shape
  | plusBracketsString _   (RIGHT shape) = rightString shape
  | plusBracketsString pts (PRETOKEN pt)  = pts pt
(* type declarations for consistency checking *)
type 'a plus_brackets = 'a plus_brackets
val _ = op bracketLexer : 'a lexer -> 'a plus_brackets lexer
(* common parsing code S92 *)
(* combinators and utilities for parsing located streams S104c *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* combinators and utilities for parsing located streams S104d *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* type declarations for consistency checking *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* combinators and utilities for parsing located streams S105a *)
fun @@ p = pair <$> srcloc <*> p
(* type declarations for consistency checking *)
val _ = op @@ : ('t, 'a) polyparser -> ('t, 'a located) polyparser
(* combinators and utilities for parsing located streams S105b *)
infix 0 <?>
fun p <?> what = p <|> errorAt ("expected " ^ what) <$>! srcloc
(* type declarations for consistency checking *)
val _ = op <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
(* combinators and utilities for parsing located streams S105c *)
infix 4 <!>
fun p <!> msg =
  fn tokens => (case p tokens
                  of SOME (OK _, unread) =>
                       (case peek srcloc tokens
                          of SOME loc => SOME (errorAt msg loc, unread)
                           | NONE => NONE)
                   | _ => NONE)
(* type declarations for consistency checking *)
val _ = op <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser
(* combinators and utilities for parsing located streams S108d *)
fun nodups (what, context) (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) = if List.exists (fn y : string => y = x) xs then
                          errorAt (what ^ " " ^ x ^ " appears twice in " ^
                                                                    context) loc
                        else
                          dup xs
  in  dup names
  end
(* type declarations for consistency checking *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* transformers for interchangeable brackets S106 *)
fun notCurly (_, CURLY) = false
  | notCurly _          = true

(* left: takes shape, succeeds or fails
   right: takes shape and
      succeeds with right bracket of correct shape
      errors with right bracket of incorrect shape
      fails with token that is not right bracket *)

fun left  tokens = ((fn (loc, LEFT  s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun right tokens = ((fn (loc, RIGHT s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun leftCurly tokens = sat (not o notCurly) left tokens

fun atRight expected = rewind right <?> expected

fun badRight msg =
  (fn (loc, shape) => errorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* transformers for interchangeable brackets S107 *)
type ('t, 'a) pb_parser = ('t plus_brackets, 'a) polyparser
datatype right_result
  = FOUND_RIGHT      of bracket_shape located
  | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
  | NO_RIGHT

fun scanToClose tokens = 
  let val loc = getOpt (peek srcloc tokens, ("end of stream", 9999))
      fun scan lpcount tokens =
        (* lpcount is the number of unmatched left parentheses *)
        case tokens
          of EOL _                  ::: tokens => scan lpcount tokens
           | INLINE (_, LEFT  t)    ::: tokens => scan (lpcount+1) tokens
           | INLINE (_, RIGHT t)    ::: tokens => if lpcount = 0 then
                                                    pure (SCANNED_TO_RIGHT loc)
                                                                          tokens
                                                  else
                                                    scan (lpcount-1) tokens
           | INLINE (_, PRETOKEN _) ::: tokens => scan lpcount tokens
           | EOS         => pure NO_RIGHT tokens
           | SUSPENDED s => scan lpcount (demand s)
  in  scan 0 tokens
  end

fun matchingRight tokens = (FOUND_RIGHT <$> right <|> scanToClose) tokens

fun matchBrackets _ (loc, left) _ NO_RIGHT =
      errorAt ("unmatched " ^ leftString left) loc
  | matchBrackets e (loc, left) _ (SCANNED_TO_RIGHT loc') =
      errorAt ("expected " ^ e) loc
  | matchBrackets _ (loc, left) a (FOUND_RIGHT (loc', right)) =
      if left = right then
        OK a
      else
        errorAt (rightString right ^ " does not match " ^ leftString left ^
                 (if loc <> loc' then " at " ^ srclocString loc else "")) loc'
(* type declarations for consistency checking *)
type right_result = right_result
val _ = op matchingRight : ('t, right_result) pb_parser
val _ = op scanToClose   : ('t, right_result) pb_parser
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
(* transformers for interchangeable brackets S108a *)
fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
(* type declarations for consistency checking *)
val _ = op bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* transformers for interchangeable brackets S108b *)
fun usageParser keyword =
  let val left = eqx #"(" one <|> eqx #"[" one
      val getkeyword = left *> (implode <$> many1 (sat (not o isDelim) one))
  in  fn (usage, p) =>
        case getkeyword (streamOfList (explode usage))
          of SOME (OK k, _) => bracketKeyword (keyword k, usage, p)
           | _ => let exception BadUsage of string in raise BadUsage usage end
  end
(* type declarations for consistency checking *)
val _ = op usageParser : (string -> ('t, string) pb_parser) -> string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* transformers for interchangeable brackets S108c *)
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* code used to debug parsers S109a *)
fun safeTokens stream =
  let fun tokens (seenEol, seenSuspended) =
            let fun get (EOL _         ::: ts) = if seenSuspended then []
                                                 else tokens (true, false) ts
                  | get (INLINE (_, t) ::: ts) = t :: get ts
                  | get  EOS                   = []
                  | get (SUSPENDED (ref (PRODUCED ts))) = get ts
                  | get (SUSPENDED s) = if seenEol then []
                                        else tokens (false, true) (demand s)
            in   get
            end
  in  tokens (false, false) stream
  end
(* type declarations for consistency checking *)
val _ = op safeTokens : 'a located eol_marked stream -> 'a list
(* code used to debug parsers S109b *)
fun showErrorInput asString p tokens =
  case p tokens
    of result as SOME (ERROR msg, rest) =>
         if String.isSubstring " [input: " msg then
           result
         else
           SOME (ERROR (msg ^ " [input: " ^
                        spaceSep (map asString (safeTokens tokens)) ^ "]"),
               rest)
     | result => result
(* type declarations for consistency checking *)
val _ = op showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
(* code used to debug parsers S109c *)
fun wrapAround tokenString what p tokens =
  let fun t tok = " " ^ tokenString tok
      val _ = app eprint ["Looking for ", what, " at"]
      val _ = app (eprint o t) (safeTokens tokens)
      val _ = eprint "\n"
      val answer = p tokens
      val _ = app eprint [case answer of NONE => "Didn't find " | SOME _ =>
                                                                       "Found ",
                         what, "\n"]
  in  answer
  end handle e => ( app eprint ["Search for ", what, " raised ", exnName e, "\n"
                                                                               ]
                  ; raise e)
(* type declarations for consistency checking *)
val _ = op wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t,
                                                                  'a) polyparser
(* streams that issue two forms of prompts S110a *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* type declarations for consistency checking *)
val _ = op echoTagStream : line stream -> line stream 
(* streams that issue two forms of prompts S110b *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* type declarations for consistency checking *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* streams that issue two forms of prompts S110c *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* type declarations for consistency checking *)
val _ = op lexLineWith : 't lexer -> line -> 't stream
(* streams that issue two forms of prompts S110d *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) = SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* type declarations for consistency checking *)
val _ = op parseWithErrors : ('t, 'a) polyparser -> 't located eol_marked stream
                                                              -> 'a error stream
(* streams that issue two forms of prompts S111 *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* type declarations for consistency checking *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* streams that issue two forms of prompts S112 *)
fun ('t, 'a) interactiveParsedStream (lexer, parser) (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref ps1
      fun setPrompt ps = fn _ => thePrompt := ps

      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      fun lexAndDecorate (loc, line) =
        let val tokens = postStream (lexLineWith lexer line, setPrompt ps2)
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val xdefs_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        (name, lines)
(* type declarations for consistency checking *)
val _ = op interactiveParsedStream : 't lexer * ('t, 'a) polyparser -> string *
                                              line stream * prompts -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> 't located eol_marked stream
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
(* common parsing code ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun ('t, 'a) finiteStreamOfLine fail (lexer, parser) line =
  let val lines = streamOfList [line] @@@ streamOfEffects fail
      fun lexAndDecorate (loc, line) =
        let val tokens = lexLineWith lexer line
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val things_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        ("command line", lines)
  in  
      stripAndReportErrors things_with_errors
  end 
val _ = finiteStreamOfLine :
          (unit -> string option) -> 't lexer * ('t, 'a) polyparser -> line ->
                                                                       'a stream
(* shared utility functions for initializing interpreters S214b *)
fun override_if_testing () =                           (*OMIT*)
  if isSome (OS.Process.getEnv "NOERRORLOC") then      (*OMIT*)
    toplevel_error_format := WITHOUT_LOCATIONS         (*OMIT*)
  else                                                 (*OMIT*)
    ()                                                 (*OMIT*)
fun setup_error_format interactivity =
  if prompts interactivity then
    toplevel_error_format := WITHOUT_LOCATIONS
    before override_if_testing () (*OMIT*)
  else
    toplevel_error_format := WITH_LOCATIONS
    before override_if_testing () (*OMIT*)
(* function application with overflow checking S74b *)
local
  val throttleCPU = case OS.Process.getEnv "BPCOPTIONS"
                      of SOME "nothrottle" => false
                       | _ => true
  val defaultRecursionLimit = 6000 (* about 1/5 of 32,000? *)
  val recursionLimit = ref defaultRecursionLimit
  val evalFuel       = ref 1000000
  datatype checkpoint = RECURSION_LIMIT of int
in
  val defaultEvalFuel = ref (!evalFuel)
  fun withFuel n f x = 
    let val old = !evalFuel
        val _ = evalFuel := n
    in  (f x before evalFuel := old) handle e => (evalFuel := old; raise e)
    end

  fun fuelRemaining () = !evalFuel

  fun checkpointLimit () = RECURSION_LIMIT (!recursionLimit)
  fun restoreLimit (RECURSION_LIMIT n) = recursionLimit := n

  fun applyCheckingOverflow f =
    if !recursionLimit <= 0 then
      raise RuntimeError "recursion too deep"
    else if throttleCPU andalso !evalFuel <= 0 then
      (evalFuel := !defaultEvalFuel; raise RuntimeError "CPU time exhausted")
    else
      let val _ = recursionLimit := !recursionLimit - 1
          val _ = evalFuel        := !evalFuel - 1
      in  fn arg => f arg before (recursionLimit := !recursionLimit + 1)
      end
  fun resetOverflowCheck () = ( recursionLimit := defaultRecursionLimit
                              ; evalFuel := !defaultEvalFuel
                              )
end
(* function [[forward]], for mutual recursion through mutable reference cells S75a *)
fun forward what _ =
  let exception UnresolvedForwardDeclaration of string
  in  raise UnresolvedForwardDeclaration what
  end
exception LeftAsExercise of string



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \USCHEME                     *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for \uscheme S207c *)
(* definitions of [[exp]] and [[value]] for \uscheme 305a *)
datatype exp = LITERAL of value
             | VAR     of name
             | SET     of name * exp
             | IFX     of exp * exp * exp
             | WHILEX  of exp * exp
             | BEGIN   of exp list
             | APPLY   of exp * exp list
             | LETX    of let_kind * (name * exp) list * exp
             | LAMBDA  of lambda
and let_kind = LET | LETREC | LETSTAR
and    value = SYM       of name
             | NUM       of real
             | BOOLV     of bool   
             | NIL
             | LUANIL
             | PAIR      of value ref * value ref
             | CLOSURE   of lambda * value ref env
             | PRIMITIVE of primitive
             | ARRAY     of value array
withtype primitive = exp * value list -> value (* raises RuntimeError *)
     and lambda    = name list * exp
(* definition of [[def]] for \uscheme 305b *)
datatype def  = VAL    of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
(* definition of [[unit_test]] for untyped languages (shared) S207a *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ASSERT of exp
                   | CHECK_ERROR  of exp
                   | CHECK_EXPECT' of value * string * value * string
(* definition of [[xdef]] (shared) S207b *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* definition of [[valueString]] for \uscheme, \tuscheme, and \nml 306 *)
fun valueString (SYM v)   = v
  | valueString (NUM n)   = realString n
  | valueString (BOOLV b) = if b then "#t" else "#f"
  | valueString (NIL)     = "()"
  | valueString (LUANIL)     = "nil"
  | valueString (PAIR (ref car, ref cdr))  = 
      let fun tail (PAIR (ref car, ref cdr)) = " " ^ valueString car ^ tail cdr
            | tail NIL = ")"
            | tail v = " . " ^ valueString v ^ ")"
(* type declarations for consistency checking *)
val _ = op valueString : value -> string
      in  "(" ^ valueString car ^ tail cdr
      end
  | valueString (CLOSURE ((xs, _), _)) = "<function(" ^ Int.toString (length xs) ^ ")>"
  | valueString (PRIMITIVE _) = "<primitive>"
  | valueString (ARRAY vs) =
    "[| " ^ String.concatWith " " (Array.foldr (fn (v, vs) => valueString v :: vs) [] vs) ^ " |]"
(* definition of [[expString]] for \uscheme S221a *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL (v as NUM   _) => valueString v
         | LITERAL (v as BOOLV _) => valueString v
         | LITERAL v => "'" ^ valueString v
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) =>
                         bracketSpace ["while", expString cond, expString body]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ["lambda", bracketSpace xs,
                                                                 expString body]
  end
(* type declarations for consistency checking *)
val _ = op valueString      : value -> string
val _ = op expString        : exp   -> string


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \USCHEME, \TUSCHEME, AND \NML\ VALUES  *)
(*                                                               *)
(*****************************************************************)

(* utility functions on \uscheme, \tuscheme, and \nml\ values 307b *)
val embedInt = NUM o Real.fromInt
val embedReal = NUM
fun embedBool b = BOOLV b
fun projectBool (BOOLV false) = false
  | projectBool (LUANIL)      = false
  | projectBool _             = true
(* type declarations for consistency checking *)
val _ = op embedBool   : bool  -> value
val _ = op projectBool : value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values 307c *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (ref h, ref (embedList t))
(* utility functions on \uscheme, \tuscheme, and \nml\ values S207d *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (LUANIL,   LUANIL )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = Real.== (n1, n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(* type declarations for consistency checking *)
val _ = op equalatoms : value * value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values S208a *)
fun equalpairs (PAIR (ref car1, ref cdr1), PAIR (ref car2, ref cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(* type declarations for consistency checking *)
val _ = op equalpairs : value * value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values S208b *)
val testEqual = equalpairs
(* type declarations for consistency checking *)
val _ = op testEqual : value * value -> bool
(* utility functions on \uscheme, \tuscheme, and \nml\ values S221b *)
fun cycleThrough xs =
  let val remaining = ref xs
      fun next () = case !remaining
                      of [] => (remaining := xs; next ())
                       | x :: xs => (remaining := xs; x)
  in  if null xs then
        raise InternalError "empty list given to cycleThrough"
      else
        next
  end
val unspecified =
  cycleThrough [BOOLV true, NUM 39.0, SYM "this value is unspecified", NIL,
                PRIMITIVE (fn _ => let exception Unspecified in raise
                                                               Unspecified end)]
(* type declarations for consistency checking *)
val _ = op cycleThrough : 'a list -> (unit -> 'a)
val _ = op unspecified  : unit -> value



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \USCHEME, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \uscheme, providing [[filexdefs]] and [[stringsxdefs]] S215b *)
(* lexical analysis for \uscheme\ and related languages S215c *)
datatype pretoken = QUOTE
                  | INT     of int
                  | REAL    of real
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* lexical analysis for \uscheme\ and related languages S215d *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (REAL x)    = Real.toString x
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* lexical analysis for \uscheme\ and related languages S216a *)
local
  (* functions used in all lexers S216c *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* type declarations for consistency checking *)
  val _ = op noneIfLineEnds : 'a lexer
  (* functions used in the lexer for \uscheme S216b *)
  fun atom "#t" = SHARP true
    | atom "#f" = SHARP false
    | atom x    = NAME x
in
  val schemeToken =
    whitespace *>
    bracketLexer   (  QUOTE   <$  eqx #"'" one
                  <|> REAL    <$> realToken isDelim
                  <|> INT     <$> intToken isDelim
                  <|> (atom o implode) <$> many1 (sat (not o isDelim) one)
                  <|> noneIfLineEnds
                   )
(* type declarations for consistency checking *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> pretoken
end
(* parsers for single \uscheme\ tokens S217a *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val realp     = (fn (REAL  x)   => SOME x  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val name      = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val any_name  = name
(* parsers and parser builders for formal parameters and bindings S217b *)
fun formalsOf what name context = 
  nodups ("formal parameter", context) <$>! @@ (bracket (what, many name))

fun bindingsOf what name exp =
  let val binding = bracket (what, pair <$> name <*> exp)
  in  bracket ("(... " ^ what ^ " ...) in bindings", many binding)
  end

fun distinctBsIn bindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map fst bs) >>=+ (fn _ => bs)
  in  check <$>! @@ bindings
  end
(* type declarations for consistency checking *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* parsers and parser builders for formal parameters and bindings S217c *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* type declarations for consistency checking *)
val _ = op recordFieldsOf : name parser -> name list parser
(* parsers and parser builders for formal parameters and bindings S217d *)
fun kw keyword = 
  eqx keyword any_name
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* type declarations for consistency checking *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* parsers and parser builders for \scheme-like syntax S218a *)
fun sexp tokens = (
     SYM       <$> (notDot <$>! @@ any_name)
 <|> embedReal <$> realp
 <|> embedInt  <$> int
 <|> embedBool <$> booltok
 <|> leftCurly <!> "curly brackets may not be used in S-expressions"
 <|> embedList <$> bracket ("list of S-expressions", many sexp)
 <|> (fn v => embedList [SYM "quote", v]) 
               <$> (quote *> sexp)
) tokens
and notDot (loc, ".") =
      errorAt "this interpreter cannot handle . in quoted S-expressions" loc
  | notDot (_,   s)   = OK s
(* type declarations for consistency checking *)
val _ = op sexp : value parser
(* parsers and parser builders for \scheme-like syntax S218b *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> embedReal <$> realp
                          <|> LITERAL <$> embedInt  <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* parsers and parser builders for \scheme-like syntax S219a *)
fun fullSchemeExpOf atomic keywordsOf =
  let val exp = fn tokens => fullSchemeExpOf atomic keywordsOf tokens
  in      atomic
      <|> keywordsOf exp
      <|> quote *> (LITERAL <$> sexp)
      <|> quote *> badRight "quote ' followed by right bracket"
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "(): unquoted empty parentheses"
      <|> bracket("function application", curry APPLY <$> exp <*> many exp)
  end
(* parsers and [[xdef]] streams for \uscheme S218c *)
fun exptable exp =
  let val bindings = bindingsOf "(x e)" name exp
      val formals  = formalsOf "(x1 x2 ...)" name "lambda"
      val dbs      = distinctBsIn bindings
(* type declarations for consistency checking *)
val _ = op exptable  : exp parser -> exp parser
val _ = op exp       : exp parser
val _ = op bindings  : (name * exp) list parser
  in usageParsers
     [ ("(if e1 e2 e3)",            curry3 IFX          <$> exp <*> exp <*> exp)
     , ("(while e1 e2)",            curry  WHILEX       <$> exp  <*> exp)
     , ("(set x e)",                curry  SET          <$> name <*> exp)
     , ("(begin e1 ...)",                  BEGIN        <$> many exp)
     , ("(lambda (names) body)",    curry  LAMBDA       <$> formals      <*> exp
                                                                               )
     , ("(let (bindings) body)",    curry3 LETX LET     <$> dbs "let"    <*> exp
                                                                               )
     , ("(letrec (bindings) body)", curry3 LETX LETREC  <$> dbs "letrec" <*> exp
                                                                               )
     , ("(let* (bindings) body)",   curry3 LETX LETSTAR <$> bindings     <*> exp
                                                                               )
     , ("(quote sexp)",             LITERAL             <$> sexp)

   (* rows added to ML \uscheme's [[exptable]] in exercises ((prototype)) 319 *)
     , ("(cond ([q a] ...))",
        let fun desugarCond qas = raise LeftAsExercise "desugar cond"
            val qa = bracket ("[question answer]", pair <$> exp <*> exp)
     (* type declarations for consistency checking *)
     val _ = op desugarCond : (exp * exp) list -> exp
        in  desugarCond <$> many qa
        end
       )
     (* rows added to ML \uscheme's [[exptable]] in exercises S218d *)
, ("(&& e1 ... en)",
    let fun andSugar [] = LITERAL (BOOLV true)
          | andSugar [e] = e
          | andSugar (e::es) = IFX (e, andSugar es, LITERAL (BOOLV false))
    in  andSugar <$> many exp
    end
  )

, ("(|| e1 ... en)",
    let fun freeIn exp y =
          let fun member y [] = false
                | member y (z::zs) = y = z orelse member y zs
              fun has_y (LITERAL _) = false
                | has_y (VAR x) = x = y
                | has_y (SET (x, e)) = x = y orelse has_y e
                | has_y (IFX (e1, e2, e3))  = List.exists has_y [e1, e2, e3]
                | has_y (WHILEX (e1, e2))   = List.exists has_y [e1, e2]
                | has_y (BEGIN es)          = List.exists has_y es
                | has_y (APPLY (e, es))     = List.exists has_y (e::es)
                | has_y (LETX (LET, bs, e)) = List.exists rhs_has_y bs orelse
                                              (not (member y (map fst bs))
                                              andalso has_y e)
                | has_y (LETX (LETSTAR, [], e)) = has_y e
                | has_y (LETX (LETSTAR, b::bs, e)) =
                    has_y (LETX (LET, [b], LETX(LETSTAR, bs, e)))
                | has_y (LETX (LETREC, bs, e)) =
                    not (member y (map fst bs)) andalso has_y e
                | has_y (LAMBDA (xs, e)) = not (member y xs) andalso has_y e
              and rhs_has_y (_, e) = has_y e
          in  has_y exp
          end

        fun orSugar [] = LITERAL (BOOLV false)
          | orSugar [e] = e
          | orSugar (e1::es) =
              let val e2 = orSugar es
                  val x_n's = streamMap (fn n => "x" ^ intString n) naturals
                  val xs = streamFilter (not o freeIn e2) x_n's
                  val (x, _)  = valOf (streamGet xs)
              in  LETX (LET, [(x, e1)], IFX (VAR x, VAR x, e2))
              end
    in  orSugar <$> many exp
    end
  )
     (* add syntactic sugar here, each row preceded by a comma *)
     ]
  end
(* parsers and [[xdef]] streams for \uscheme S219b *)
val exp = fullSchemeExpOf (atomicSchemeExpOf name) exptable
(* parsers and [[xdef]] streams for \uscheme S219c *)
val deftable = usageParsers
  [ ("(define f (args) body)",
        let val formals  = formalsOf "(x1 x2 ...)" name "define"
        in  curry DEFINE <$> name <*> (pair <$> formals <*> exp)
        end)
  , ("(val x e)", curry VAL <$> name <*> exp)
  ]
(* type declarations for consistency checking *)
val _ = op deftable  : def parser
(* parsers and [[xdef]] streams for \uscheme S219d *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  ]
(* type declarations for consistency checking *)
val _ = op testtable : unit_test parser
(* parsers and [[xdef]] streams for \uscheme S219e *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> name)
  (* rows added to \uscheme\ [[xdeftable]] in exercises S219f *)
  (* add syntactic sugar here, each row preceded by a comma *) 
  , ( "(record record-name (field-name ...))"
    , let fun arityError n args =
            raise RuntimeError ("primitive function expected " ^ intString n ^
                                " arguments; got " ^ intString (length args))
          fun noExp f (e, vs) = f vs  (* inExp & friends not in scope here *)
          fun binaryOp f = noExp (fn [a, b] => f (a, b) | args => arityError 2
                                                                           args)
          fun unaryOp  f = noExp (fn [a]    => f a      | args => arityError 1
                                                                           args)

          fun uprim f e        = APPLY (LITERAL (PRIMITIVE (unaryOp f)), [e])
          fun bprim f (e1, e2) = APPLY (LITERAL (PRIMITIVE (binaryOp f)), [e1,
                                                                            e2])

          val car = uprim (fn PAIR (ref x, _) => x  | _ => raise RuntimeError
                                                                     "non-pair")
          val setcar = bprim (fn (PAIR (c, _), v) => (c := v; NIL)
                               | _ => raise RuntimeError "non-pair")

          val cdr = uprim (fn PAIR (ref x, ref xs) => xs | _ => raise RuntimeError
                                                                     "non-pair")
          val nullp = uprim (BOOLV o (fn NIL    => true | _ => false))
          val pairp = uprim (BOOLV o (fn PAIR _ => true | _ => false))
          val cons = bprim (fn (x, y) => PAIR (ref x, ref y))

          fun desugarRecord recname fieldnames =
                recordConstructor recname fieldnames ::
                recordPredicate recname fieldnames ::
                recordAccessors recname 0 fieldnames @
                recordMutators recname 0 fieldnames
          and recordConstructor recname fieldnames = 
                let val con = "make-" ^ recname
                    val formals = map (fn s => "the-" ^ s) fieldnames
                    val body = cons (LITERAL (SYM con), varlist formals)
                in  DEFINE (con, (formals, body))
                end
          and recordPredicate recname fieldnames =
                let val tag = SYM ("make-" ^ recname)
                    val predname = recname ^ "?"
                    val r = VAR "r" : exp
                    val formals = ["r"]
                    val good_car = APPLY (VAR "=", [car r, LITERAL tag])
                    fun good_cdr looking_at [] = nullp looking_at
                      | good_cdr looking_at (_ :: rest) =
                          and_also (pairp looking_at, good_cdr (cdr looking_at)
                                                                           rest)
                    val body =
                      and_also (pairp r, and_also (good_car, good_cdr (cdr r)
                                                                    fieldnames))
                in  DEFINE (predname, (formals, body))
                end
          and recordAccessors recname n [] = []
            | recordAccessors recname n (field::fields) =
                let val predname = recname ^ "?"
                    val accname = recname ^ "-" ^ field
                    val formals = ["r"]
                    val thefield = car (cdrs (n+1, VAR "r"))
                    val body = IFX ( APPLY (VAR predname, [VAR "r"])
                                   , thefield
                                   , error (list [ SYM "value-passed-to"
                                                 , SYM accname
                                                 , SYM "is-not-a"
                                                 , SYM recname
                                                 ]))
                in  DEFINE (accname, (formals, body)) ::
                    recordAccessors recname (n+1) fields
                end
          and recordMutators recname n [] = []
            | recordMutators recname n (field::fields) =
                let val predname = recname ^ "?"
                    val mutname = "set-" ^ recname ^ "-" ^ field ^ "!"
                    val formals = ["r", "v"]
                    val setfield = setcar (cdrs (n+1, VAR "r"), VAR "v")
                    val body = IFX ( APPLY (VAR predname, [VAR "r"])
                                   , setfield
                                   , error (list [ SYM "value-passed-to"
                                                 , SYM mutname
                                                 , SYM "is-not-a"
                                                 , SYM recname
                                                 ]))
                in  DEFINE (mutname, (formals, body)) ::
                    recordMutators recname (n+1) fields
                end
          and and_also (p, q) = IFX (p, q, LITERAL (BOOLV false)) : exp
          and cdrs (0, xs) = xs
            | cdrs (n, xs) = cdr (cdrs (n-1, xs))

          and list [] = LITERAL NIL
            | list (v::vs) = cons (LITERAL v, list vs)
          and error x = APPLY (VAR "error", [x])

          and varlist [] = LITERAL NIL
            | varlist (x::xs) = cons (VAR x, varlist xs)
      in  DEFS <$> (desugarRecord <$> name <*> recordFieldsOf name)
      end
    ) 
  ]
(* type declarations for consistency checking *)
val _ = op xdeftable : xdef parser
(* parsers and [[xdef]] streams for \uscheme S219g *)
val xdef =  DEF  <$> deftable
        <|> TEST <$> testtable
        <|>          xdeftable
        <|> badRight "unexpected right bracket"
        <|> DEF <$> EXP <$> exp
        <?> "definition"
(* type declarations for consistency checking *)
val _ = op xdef : xdef parser
(* parsers and [[xdef]] streams for \uscheme S220a *)
val xdefstream = 
  interactiveParsedStream (schemeToken, xdef)
(* type declarations for consistency checking *)
val _ = op xdefstream : string * line stream * prompts -> xdef stream
(* shared definitions of [[filexdefs]] and [[stringsxdefs]] S86b *)
fun filexdefs (filename, fd, prompts) = xdefstream (filename, filelines fd,
                                                                        prompts)
fun stringsxdefs (name, strings) = xdefstream (name, streamOfList strings,
                                                                      noPrompts)
(* type declarations for consistency checking *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \USCHEME *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \uscheme S211a *)


  fun free e =
    let fun addFree (LITERAL v, s)  = s
          | addFree (VAR x, s)      = insert (x, s)
          | addFree (SET (x, e), s) = insert (x, addFree (e, s))
          | addFree (IFX (e1, e2, e3), s) = foldl addFree s [e1, e2, e3]
          | addFree (WHILEX (e1, e2), s)   = foldl addFree s [e1, e2]
          | addFree (BEGIN es, s)          = foldl addFree s es
          | addFree (APPLY (e, es), s)   = foldl addFree s (e::es)
          | addFree (LAMBDA (xs, body), s) = union (s, diff (free body, xs))
          | addFree (LETX (LET, bs, body), s) = 
              let val (xs, es) = ListPair.unzip bs
                  val s = foldl addFree s es
                  val bfree = diff (free body, xs)
              in  union (s, bfree)
              end
          | addFree (LETX (LETREC, bs, body), s) = 
              let val (xs, es) = ListPair.unzip bs
                  val bfree = foldl addFree (free body) es
              in  union (s, diff (bfree, xs))
              end
          | addFree (LETX (LETSTAR, [], body), s) = s
          | addFree (LETX (LETSTAR, (x, e) :: bs, body), s) =
              union (addFree (e, s), diff (addFree (body, []), [x]))

    in  addFree (e, [])
    end




(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
(* definitions of [[eval]] and [[evaldef]] for \uscheme 307d *)
fun eval (e, rho) =
  let val go = applyCheckingOverflow id in go end (* OMIT *)
  let fun ev (LITERAL v) = v
        (* more alternatives for [[ev]] for \uscheme 308a *)
        | ev (VAR x) = !(find (x, rho))
        | ev (SET (x, e)) = 
            let val v = ev e
            in  find (x, rho) := v;
                v
            end
        (* more alternatives for [[ev]] for \uscheme 308b *)
        | ev (IFX (e1, e2, e3)) = ev (if projectBool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if projectBool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              BOOLV false
        (* more alternatives for [[ev]] for \uscheme 308c *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, BOOLV false)
            end
        (* more alternatives for [[ev]] for \uscheme 308d *)
        | ev (LAMBDA (xs, e)) = CLOSURE ((xs, e), rho)
        (* more alternatives for [[ev]] for \uscheme 308e *)
        | ev (e as APPLY (f, args)) = 
               (case ev f
                  of PRIMITIVE prim => prim (e, map ev args)
                   | closure as PAIR (ref (f as CLOSURE _), _) =>
                       ev (APPLY (LITERAL f, LITERAL closure :: args))
                   | CLOSURE clo    =>
                       (* apply closure [[clo]] to [[args]] ((mlscheme)) 309a *)
                                       let val ((formals, body), savedrho) = clo
                                           val actuals = map ev args
                                       in  eval (body, bindList (formals, map
                                                         ref actuals, savedrho))
                                           handle BindListLength => 
                                               raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                      "expected (" ^ spaceSep formals ^ ")" ^
                                      " but got (" ^ spaceSep (map valueString actuals) ^ ")"
                                               )
                                       end
                   | v => raise RuntimeError ("Applied non-function " ^
                                                                  valueString v)
               )
        (* more alternatives for [[ev]] for \uscheme 309b *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
        (* type declarations for consistency checking *)
        val _ = ListPair.unzip : ('a * 'b) list -> 'a list * 'b list
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((n, e), rho) = bind (n, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* more alternatives for [[ev]] for \uscheme 309c *)
        | ev (LETX (LETREC, bs, body)) =
            let val (names, values) = ListPair.unzip bs

(* if any expression in [[values]] is not a [[lambda]], reject the [[letrec]] S208g *)
                fun insistLambda (LAMBDA _) = ()
                  | insistLambda e =
                      raise RuntimeError (
                                 "letrec tries to bind non-lambda expression " ^
                                          expString e)
                val _ = app insistLambda values
                val rho' =
                  bindList (names, map (fn _ => ref (unspecified())) values, rho
                                                                               )
                val updates = map (fn (n, e) => (n, eval (e, rho'))) bs
        (* type declarations for consistency checking *)
        val _ = List.app : ('a -> unit) -> 'a list -> unit
            in  List.app (fn (n, v) => find (n, rho') := v) updates; 
                eval (body, rho')
            end
(* type declarations for consistency checking *)
val _ = op embedList : value list -> value
(* type declarations for consistency checking *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
  in  ev e
  end

(* definitions of [[eval]] and [[evaldef]] for \uscheme 310b *)
fun evaldef (VAL (x, e), rho) =
      let val v   = eval (e, rho)
          val _   = find (x, rho) := v
          val response = case e of LAMBDA _ => x
                                 | _ => valueString v
(* type declarations for consistency checking *)
val _ = op evaldef : def  * value ref env -> value ref env * string
      in  (rho, response)
      end
(* definitions of [[eval]] and [[evaldef]] for \uscheme 310c *)
  | evaldef (EXP e, rho) =        
      let val v = eval (e, rho)
      in  (rho, valueString v)
      end
  | evaldef (DEFINE (f, lambda), rho) =
      let val (xs, e) = lambda
      in  evaldef (VAL (f, LAMBDA lambda), rho)
      end
(* definitions of [[eval]], [[evaldef]], [[basis]], and [[processDef]] for \uscheme S212c *)

fun freedef (VAL (x, e)) = insert (x, free e)
  | freedef (EXP e) = free e
  | freedef (DEFINE (f, lambda)) = insert (f, free (LAMBDA lambda))

fun freetest (CHECK_EXPECT (e, e')) = union (free e, free e')
  | freetest (CHECK_EXPECT' _) = []
  | freetest (CHECK_ASSERT e) = free e
  | freetest (CHECK_ERROR e)  = free e

fun ensureBound (x, rho) =
  (find (x, rho); rho)
  handle NotFound _ => bind (x, ref LUANIL, rho)

type basis = value ref env
fun processDef (d, rho, interactivity) =
  let val rho = foldl ensureBound rho (freedef d)
      val (rho', response) = evaldef (d, rho)
      val _ = if prints interactivity then println response else ()
  in  rho'
  end
fun dump_names basis = app (println o fst) basis  (*OMIT*)
(* shared unit-testing utilities S78e *)
fun failtest strings = (app eprint strings; eprint "\n"; false)
(* shared unit-testing utilities S79a *)
fun reportTestResultsOf what (npassed, nthings) =
  case (npassed, nthings)
    of (_, 0) => ()  (* no report *)
     | (0, 1) => println ("The only " ^ what ^ " failed.")
     | (1, 1) => println ("The only " ^ what ^ " passed.")
     | (0, 2) => println ("Both " ^ what ^ "s failed.")
     | (1, 2) => println ("One of two " ^ what ^ "s passed.")
     | (2, 2) => println ("Both " ^ what ^ "s passed.")
     | _ => if npassed = nthings then
               app print ["All ", intString nthings, " " ^ what ^ "s passed.\n"]
            else if npassed = 0 then
               app print ["All ", intString nthings, " " ^ what ^ "s failed.\n"]
            else
               app print [intString npassed, " of ", intString nthings,
                          " " ^ what ^ "s passed.\n"]
val reportTestResults = reportTestResultsOf "test"
(* shared definition of [[withHandlers]] S213a *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn) a (fn s => caught (fillAtLoc (s, loc
                                                                             )))

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] S213b *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
(* definition of [[testIsGood]] for \uscheme S220b *)
fun testIsGood (test, rho) =
  let val rho = foldl ensureBound rho (freetest test)
      fun outcome e = withHandlers (fn e => OK (eval (e, rho))) e (ERROR o
                                                                     stripAtLoc)
     
   (* [[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml S220c *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (* type declarations for consistency checking *)
      val _ = op asSyntacticValue : exp -> value option

  (* shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]] S78d *)
      (* shared [[whatWasExpected]] S77 *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (* type declarations for consistency checking *)
      val _ = op whatWasExpected  : exp * value error -> string
      val _ = op asSyntacticValue : exp -> value option
      (* shared [[checkExpectPassesWith]], which calls [[outcome]] S78a *)
      val cxfailed = "Check-expect failed:"
      fun checkExpectPassesWith equals (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               equals (check, expect) orelse
               failtest [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                         whatWasExpected (expectx, OK expect), ", but it's ",
                         valueString check, "."]
           | (ERROR msg, tried) =>
               failtest [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                         whatWasExpected (expectx, tried), ", but evaluating ",
                         expString checkx, " caused this error: ", msg]
           | (_, ERROR msg) =>
               failtest [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                         whatWasExpected (expectx, ERROR msg),
                                                            ", but evaluating ",
                         expString expectx, " caused this error: ", msg]
      (* type declarations for consistency checking *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      fun checkExpect'PassesWith equals (checkv, checks, expectv, expects) =
          equals (checkv, expectv) orelse
          failtest [cxfailed, " expected ", checks,
                    " to evaluate to ", valueString expectv, ", but it's ",
                         valueString checkv, "."]
      (* type declarations for consistency checking *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      val _ = op outcome  : exp -> value error
      val _ = op failtest : string list -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] S78b *)
      val cafailed = "check-assert failed: "
      fun checkAssertPasses checkx =
            case outcome checkx
              of OK check => projectBool check orelse
                             failtest [cafailed, " expected assertion ",
                                                               expString checkx,
                                       " to hold, but it doesn't"]
               | ERROR msg =>
                   failtest [cafailed, " expected assertion ", expString checkx,
                             " to hold, but evaluating it caused this error: ",
                                                                            msg]
      (* type declarations for consistency checking *)
      val _ = op checkAssertPasses : exp -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] S78c *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (* type declarations for consistency checking *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEqual (cx, ex)
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_EXPECT' args)  = checkExpect'PassesWith testEqual args
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
(* type declarations for consistency checking *)
val _ = op testIsGood : unit_test * basis -> bool
val _ = op outcome    : exp -> value error
  in  passes test
  end
(* shared definition of [[processTests]] S79b *)
fun numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
(* type declarations for consistency checking *)
val _ = op processTests : unit_test list * basis -> unit
(* shared read-eval-print loop and [[processPredefined]] S210d *)
fun processPredefined (def,basis) = 
  processDef (def, basis, noninteractive)
(* type declarations for consistency checking *)
val _ = op noninteractive    : interactivity
val _ = op processPredefined : def * basis -> basis
(* shared read-eval-print loop and [[processPredefined]] S211b *)

val currentUnitTests : unit_test list ref option ref = ref NONE

fun addUnitTest t =
   case !currentUnitTests
     of SOME tests => tests := t :: !tests
      | NONE => raise InternalError "add unit test, but no list"

val pendingCheck : (value * string) option ref = ref NONE

fun check (v, v') =
  case !pendingCheck
   of NONE => pendingCheck := SOME (v, valueString v')
    | SOME _ => raise RuntimeError "two checks with no expect"

fun expect (v, v') =
  case !pendingCheck
   of NONE => raise RuntimeError "expect with no check"
    | SOME (check, s) => ( addUnitTest (CHECK_EXPECT' (check, s, v, valueString v'))
                         ; pendingCheck := NONE
                         )

val check  = fn x => (check  x; LUANIL)
val expect = fn x => (expect x; LUANIL)

exception Halt

fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []
      val oldUnitTests = !currentUnitTests
      val _ = currentUnitTests := SOME unitTests

(* definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]] S212b *)
      fun processXDef (xd, basis) =
        let (* definition of [[useFile]], to read from a file S212a *)
            fun useFile filename =
              let val fd = TextIO.openIn filename
                  val (_, printing) = interactivity
                  val inter' = (NOT_PROMPTING, printing)
              in  readEvalPrintWith errmsg (filexdefs (filename, fd, noPrompts),
                                                                  basis, inter')
                  before TextIO.closeIn fd
              end
            fun try (USE filename) = useFile filename
              | try (TEST t)       = (unitTests := t :: !unitTests; basis)
              | try (DEF def)      = processDef (def, basis, interactivity)
              | try (DEFS ds)      = foldl processXDef basis (map DEF ds)
                                                                        (*OMIT*)
            fun caught msg = (errmsg (stripAtLoc msg); basis)
            val _ = resetOverflowCheck ()     (* OMIT *)
        in  withHandlers try xd caught handle Halt => basis
        end 
      (* type declarations for consistency checking *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* type declarations for consistency checking *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
      val _ = currentUnitTests := oldUnitTests
  in  basis
  end
(* type declarations for consistency checking *)
type basis = basis
val _ = op processDef   : def * basis * interactivity -> basis
val _ = op testIsGood   : unit_test      * basis -> bool
val _ = op processTests : unit_test list * basis -> unit



(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \USCHEME\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \uscheme\ primitives and definition of [[initialBasis]] S214a *)
(* utility functions for building primitives in \uscheme 311a *)
fun inExp f = 
  fn (e, vs) => f vs
                handle RuntimeError msg =>
                  raise RuntimeError ("in " ^ expString e ^ ", " ^ msg)
(* type declarations for consistency checking *)
val _ = op inExp : (value list -> value) -> (exp * value list -> value)
(* utility functions for building primitives in \uscheme 311b *)
fun arityError n args =
  raise RuntimeError ("expected " ^ intString n ^
                      " but got " ^ intString (length args) ^ " arguments")
fun unaryOp  f = (fn [a]    => f a      | args => arityError 1 args)
fun binaryOp f = (fn [a, b] => f (a, b) | args => arityError 2 args)
fun nullaryOp f = (fn []    => f () | args => arityError 0 args)
(* type declarations for consistency checking *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* utility functions for building primitives in \uscheme 312a *)
fun arithOp f = binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                           | (NUM n, v) =>
                                       (* report [[v]] is not an integer 312d *)
                                           raise RuntimeError (
                                "expected a number, but got " ^ valueString v)
                           | (v, _)     =>
                                       (* report [[v]] is not an integer 312d *)
                                           raise RuntimeError (
                                "expected a number, but got " ^ valueString v)
                         )
fun arith1 f = unaryOp (fn (NUM x) => NUM (f x)
                       | v => raise RuntimeError
                                    ("expected a number, but got " ^ valueString v))

(* type declarations for consistency checking *)
val _ = op arithOp: (real * real -> real) -> (value list -> value)
(* utility functions for building primitives in \uscheme 312c *)
fun predOp f     = unaryOp  (embedBool o f)
fun comparison f = binaryOp (embedBool o f)
fun realcompare f = comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                                | (NUM n, v) =>
                                       (* report [[v]] is not a number 312d *)
                                                raise RuntimeError (
                                "expected a number, but got " ^ valueString v)
                                | (v, _)     =>
                                       (* report [[v]] is not a number 312d *)
                                                raise RuntimeError (
                                "expected a number, but got " ^ valueString v)
                              )
(* type declarations for consistency checking *)
val _ = op predOp     : (value         -> bool) -> (value list -> value)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op realcompare : (real   * real   -> bool) -> (value list -> value)
(* utility functions for building primitives in \uscheme S209e *)
fun errorPrimitive (_, [v]) = raise RuntimeError (valueString v)
  | errorPrimitive (e, vs)  = inExp (arityError 1) (e, vs)
(* type declarations for consistency checking *)
val _ = op errorPrimitive : exp * value list -> value list
fun idiv (x, y) = real (Real.floor x div Real.floor y)

fun makeArray (n, v) = ARRAY (Array.tabulate (n, (fn _ => v)))
fun arrayLength a = (NUM o real) (Array.length a)
fun arrayAt (a, i) = 
  Array.sub (a, i) handle Subscript => raise RuntimeError "array subscript out of bounds"
fun arrayAtPut (a, i, v) = 
  Array.update (a, i, v) handle Subscript => raise RuntimeError "array subscript out of bounds"
fun singletonArray v = ARRAY (Array.tabulate (1, (fn _ => v)))


val primitiveBasis =
  let val rho =
        foldl (fn ((name, prim), rho) => bind (name, ref (PRIMITIVE (inExp prim)
                                                                        ), rho))
              emptyEnv ((* primitives for \uscheme\ [[::]] 312b *)
                        ("+", arithOp op +  ) :: 
                        ("-", arithOp op -  ) :: 
                        ("*", arithOp op *  ) :: 
                        ("/", arithOp op /  ) ::
                        ("idiv", arithOp idiv  ) ::
                        ("sqrt", arith1 Math.sqrt) ::
                        ("sin", arith1 Math.sin) ::
                        ("cos", arith1 Math.cos) ::
                        ("tan", arith1 Math.tan) ::
                        ("asin", arith1 Math.asin) ::
                        ("acos", arith1 Math.acos) ::
                        ("atan", arith1 Math.atan) ::
                        ("ln", arith1 Math.ln) ::
                        ("exp", arith1 Math.exp) ::
                        (* primitives for \uscheme\ [[::]] 312e *)
                        ("<", realcompare op <) :: 
                        (">", realcompare op >) ::
                        ("=", comparison equalatoms) ::
                        ("nil?",    predOp (fn (LUANIL ) => true | _ => false))
                                                                              ::
                        ("null?",    predOp (fn (NIL    ) => true | _ => false))
                                                                              ::
                        ("boolean?", predOp (fn (BOOLV _) => true | _ => false))
                                                                              ::
                        (* primitives for \uscheme\ [[::]] S209a *)
                        ("number?",  predOp (fn (NUM   _) => true | _ => false))
                                                                              ::
                        ("symbol?",  predOp (fn (SYM   _) => true | _ => false))
                                                                              ::
                        ("pair?",    predOp (fn (PAIR  _) => true | _ => false))
                                                                              ::
                        ("function?",
                              predOp (fn (PRIMITIVE _) => true | (CLOSURE  _) =>
                                                          true | _ => false)) ::
                        (* primitives for \uscheme\ [[::]] S209b *)
                        ("mkclosure", binaryOp (fn (a, b) => PAIR (ref a, ref b))) ::
                        ("cons", binaryOp (fn (a, b) => PAIR (ref a, ref b))) ::
                        ("car",  unaryOp  (fn (PAIR (ref car, _)) => car 
                                            | NIL => raise RuntimeError
                                                     "car applied to empty list"
                                            | v => raise RuntimeError
                                                           (
                                "car applied to non-list " ^ valueString v))) ::
                        ("cdr",  unaryOp  (fn (PAIR (_, ref cdr)) => cdr 
                                            | NIL => raise RuntimeError
                                                     "cdr applied to empty list"
                                            | v => raise RuntimeError
                                                           (
                                "cdr applied to non-list " ^ valueString v))) ::
                        ("set-car!", binaryOp  (fn (PAIR (car, _), v) => (car := v; v)
                                            | (NIL, _) => raise RuntimeError
                                                     "set-car! applied to empty list"
                                            | (v, _) => raise RuntimeError
                                                           (
                                "set-car! applied to non-list " ^ valueString v))) ::
                        ("set-cdr!", binaryOp  (fn (PAIR (_, cdr), v) => (cdr := v; v)
                                            | (NIL, _) => raise RuntimeError
                                                     "set-cdr! applied to empty list"
                                            | (v, _) => raise RuntimeError
                                                           (
                                "set-cdr! applied to non-list " ^ valueString v))) ::

("Array.new", (fn [NUM n, v] => makeArray (round n, v)
                | [NUM n] =>    makeArray (round n, NIL)
                | _ => raise RuntimeError "wrong number of argumetns to Array.new"))::
("Array.new'", (fn [NUM n] =>    makeArray (round n, NIL)
                | _ => raise RuntimeError "wrong number of argumetns to Array.new'"))::
("Array.update", (fn [ARRAY a, NUM i, v] => (arrayAtPut (a, round i, v); NIL)
                | _ => raise RuntimeError "bad args to Array.update")) ::
("Array.sub", binaryOp (fn (ARRAY a, NUM i) => arrayAt (a, round i)
                         | _ => raise RuntimeError "bad args to Array.at-put")) ::
("Array.length", unaryOp (fn ARRAY a => arrayLength a 
                         | _ => raise RuntimeError "non-array to Array.length")) ::


                        ("expect",  binaryOp expect) ::
                        ("check",   binaryOp check) ::
                        ("halt",   nullaryOp (fn () => raise Halt)) ::
                        (* primitives for \uscheme\ [[::]] S209c *)
                        ("println", unaryOp (fn v => (print (valueString v ^
                                                                  "\n"); v))) ::
                        ("print",   unaryOp (fn v => (print (valueString v);
                                                                         v))) ::
                        ("printu",  unaryOp (fn NUM n => (printUTF8 (Real.floor n); NUM n)
                                              | v => raise RuntimeError (
                                                                 valueString v ^

                                            " is not a Unicode code point"))) ::
                        (* primitives for \uscheme\ [[::]] S209d *)
                        ("hash",  unaryOp (fn SYM s => embedInt (fnvHash s)
                                            | v => raise RuntimeError (
                                                                 valueString v ^

                                                    " is not a symbol"))) :: [])
      val rho = bind ("error", ref (PRIMITIVE errorPrimitive), rho)
  in  rho
  end


      val fundefs = 
                     [ ";  predefined uScheme functions 96a "
                     , "(define caar (xs) (car (car xs)))"
                     , "(define cadr (xs) (car (cdr xs)))"
                     , "(define cdar (xs) (cdr (car xs)))"
                     ,
";  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
                     ,
                 ";  more predefined combinations of [[car]] and [[cdr]] S151b "
                     , "(define cddr  (sx) (cdr (cdr  sx)))"
                     , "(define caaar (sx) (car (caar sx)))"
                     , "(define caadr (sx) (car (cadr sx)))"
                     , "(define cadar (sx) (car (cdar sx)))"
                     , "(define caddr (sx) (car (cddr sx)))"
                     , "(define cdaar (sx) (cdr (caar sx)))"
                     , "(define cdadr (sx) (cdr (cadr sx)))"
                     , "(define cddar (sx) (cdr (cdar sx)))"
                     , "(define cdddr (sx) (cdr (cddr sx)))"
                     ,
                 ";  more predefined combinations of [[car]] and [[cdr]] S151c "
                     , "(define caaaar (sx) (car (caaar sx)))"
                     , "(define caaadr (sx) (car (caadr sx)))"
                     , "(define caadar (sx) (car (cadar sx)))"
                     , "(define caaddr (sx) (car (caddr sx)))"
                     , "(define cadaar (sx) (car (cdaar sx)))"
                     , "(define cadadr (sx) (car (cdadr sx)))"
                     , "(define caddar (sx) (car (cddar sx)))"
                     , "(define cadddr (sx) (car (cdddr sx)))"
                     ,
                 ";  more predefined combinations of [[car]] and [[cdr]] S151d "
                     , "(define cdaaar (sx) (cdr (caaar sx)))"
                     , "(define cdaadr (sx) (cdr (caadr sx)))"
                     , "(define cdadar (sx) (cdr (cadar sx)))"
                     , "(define cdaddr (sx) (cdr (caddr sx)))"
                     , "(define cddaar (sx) (cdr (cdaar sx)))"
                     , "(define cddadr (sx) (cdr (cdadr sx)))"
                     , "(define cdddar (sx) (cdr (cddar sx)))"
                     , "(define cddddr (sx) (cdr (cdddr sx)))"
                     , ";  predefined uScheme functions 96b "
                     , "(define list1 (x)     (cons x '()))"
                     , "(define list2 (x y)   (cons x (list1 y)))"
                     , "(define list3 (x y z) (cons x (list2 y z)))"
                     , ";  predefined uScheme functions 99b "
                     , "(define append (xs ys)"
                     , "  (if (null? xs)"
                     , "     ys"
                     , "     (cons (car xs) (append (cdr xs) ys))))"
                     , ";  predefined uScheme functions 100b "
                     , "(define revapp (xs ys) ; (reverse xs) followed by ys"
                     , "  (if (null? xs)"
                     , "     ys"
                     , "     (revapp (cdr xs) (cons (car xs) ys))))"
                     , ";  predefined uScheme functions 101a "
                     , "(define reverse (xs) (revapp xs '()))"
                     ,
";  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
                     , "(define nth (n xs)"
                     , "  (if (= n 0)"
                     , "    (car xs)"
                     , "    (nth (- n 1) (cdr xs))))"
                     , ""
                     , "(define CAPTURED-IN (i xs) (nth (+ i 1) xs))"
                     , 
";  definitions of predefined uScheme functions [[and]], [[or]], and [[not]] 97a "
                     , "(define and (b c) (if b  c  b))"
                     , "(define or  (b c) (if b  b  c))"
                     , "(define not (b)   (if b #f #t))"
                     , ";  predefined uScheme functions 102c "
                     ,
"(define atom? (x) (or (symbol? x) (or (number? x) (or (boolean? x) (null? x)))))"
                     , ";  predefined uScheme functions 103b "
                     , "(define equal? (sx1 sx2)"
                     , "  (if (atom? sx1)"
                     , "    (= sx1 sx2)"
                     , "    (if (atom? sx2)"
                     , "        #f"
                     , "        (and (equal? (car sx1) (car sx2))"
                     , "             (equal? (cdr sx1) (cdr sx2))))))"
                     , ";  predefined uScheme functions 105c "
                     , "(define make-alist-pair (k a) (list2 k a))"
                     , "(define alist-pair-key        (pair)  (car  pair))"
                     , "(define alist-pair-attribute  (pair)  (cadr pair))"
                     , ";  predefined uScheme functions 105d "
                     ,
     "(define alist-first-key       (alist) (alist-pair-key       (car alist)))"
                     ,
     "(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))"
                     , ";  predefined uScheme functions 106a "
                     , "(define bind (k a alist)"
                     , "  (if (null? alist)"
                     , "    (list1 (make-alist-pair k a))"
                     , "    (if (equal? k (alist-first-key alist))"
                     , "      (cons (make-alist-pair k a) (cdr alist))"
                     , "      (cons (car alist) (bind k a (cdr alist))))))"
                     , "(define find (k alist)"
                     , "  (if (null? alist)"
                     , "    '()"
                     , "    (if (equal? k (alist-first-key alist))"
                     , "      (alist-first-attribute alist)"
                     , "      (find k (cdr alist)))))"
                     , ";  predefined uScheme functions 125a "
                     ,
    "(define o (f g) (lambda (x) (f (g x))))          ; ((o f g) x) = (f (g x))"
                     , ";  predefined uScheme functions 126b "
                     , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
                     , "(define uncurry (f) (lambda (x y) ((f x) y)))"
                     , ";  predefined uScheme functions 129a "
                     , "(define filter (p? xs)"
                     , "  (if (null? xs)"
                     , "    '()"
                     , "    (if (p? (car xs))"
                     , "      (cons (car xs) (filter p? (cdr xs)))"
                     , "      (filter p? (cdr xs)))))"
                     , ";  predefined uScheme functions 129b "
                     , "(define map (f xs)"
                     , "  (if (null? xs)"
                     , "    '()"
                     , "    (cons (f (car xs)) (map f (cdr xs)))))"
                     , ";  predefined uScheme functions 130a "
                     , "(define app (f xs)"
                     , "  (if (null? xs)"
                     , "    #f"
                     , "    (begin (f (car xs)) (app f (cdr xs)))))"
                     , ";  predefined uScheme functions 130b "
                     , "(define exists? (p? xs)"
                     , "  (if (null? xs)"
                     , "    #f"
                     , "    (if (p? (car xs)) "
                     , "      #t"
                     , "      (exists? p? (cdr xs)))))"
                     , "(define all? (p? xs)"
                     , "  (if (null? xs)"
                     , "    #t"
                     , "    (if (p? (car xs))"
                     , "      (all? p? (cdr xs))"
                     , "      #f)))"
                     , ";  predefined uScheme functions 130d "
                     , "(define foldr (op zero xs)"
                     , "  (if (null? xs)"
                     , "    zero"
                     , "    (op (car xs) (foldr op zero (cdr xs)))))"
                     , "(define foldl (op zero xs)"
                     , "  (if (null? xs)"
                     , "    zero"
                     , "    (foldl op (op (car xs) zero) (cdr xs))))"
                     , ";  predefined uScheme functions S150c "
                     , "(val newline      10)   (val left-round    40)"
                     , "(val space        32)   (val right-round   41)"
                     , "(val semicolon    59)   (val left-curly   123)"
                     , "(val quotemark    39)   (val right-curly  125)"
                     , "                        (val left-square   91)"
                     , "                        (val right-square  93)"
                     , ";  predefined uScheme functions S150d "
                     , "(define <= (x y) (not (> x y)))"
                     , "(define >= (x y) (not (< x y)))"
                     , "(define != (x y) (not (= x y)))"
                     , ";  predefined uScheme functions S150e "
                     , "(define max (x y) (if (> x y) x y))"
                     , "(define min (x y) (if (< x y) x y))"
                     , ";  predefined uScheme functions S151a "
                     , "(define negated (n) (- 0 n))"
                     , "(define mod (m n) (- m (* n (idiv m n))))"
                     , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
                     , "(define lcm (m n) (if (= m 0) 0 (* m (idiv n (gcd m n)))))"
                     , ";  predefined uScheme functions S151e "
                     , "(define list4 (x y z a)         (cons x (list3 y z a)))"
                     ,
                     "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
                     ,
                   "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
                     ,
                 "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
                     ,
               "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"

, "(define assoc (v pairs)"
, "  (if (null? pairs)"
, "      #f"
, "      (let* ([first (car pairs)]"
, "            [rest (cdr pairs)])"
, "        (if (equal? v (car first))"
, "            first"
, "            (assoc v rest)))))"
, ""
, "(define Table.new ()"
, "  (cons nil '()))"
, ""
, "(define Table.get (t k)"
, "  (let ([pair (assoc k (cdr t))])"
, "    (if pair"
, "        (cdr pair)"
, "        nil)))"
, ""
, "(define Table.put (t k v)"
, "  (let ([pair (assoc k (cdr t))])"
, "    (if pair"
, "        (set-cdr! pair v)"
, "        (set-cdr! t (cons (cons k v) (cdr t))))))"

                      ]


val initialBasis =
  let val rho = primitiveBasis
      val xdefs = stringsxdefs ("predefined functions", fundefs)
  in  readEvalPrintWith predefinedFunctionError (xdefs, rho, noninteractive)
  end
(* type declarations for consistency checking *)
val _ = op withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
(* type declarations for consistency checking *)
val _ = op initialBasis : basis


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNAS]], WHICH EVALUATES STANDARD INPUT GIVEN [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* function [[runAs]], which evaluates standard input given [[initialBasis]] S214c *)
val currentBasis = ref initialBasis
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs ("standard input", TextIO.stdIn, prompts)
  in  currentBasis := readEvalPrintWith eprintln (xdefs, !currentBasis, interactivity)
  end 
(* type declarations for consistency checking *)
val _ = op runAs : interactivity -> unit

fun runPathAs interactivity "-" = runAs interactivity
  | runPathAs interactivity path =
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val fd = TextIO.openIn path
      val xdefs = filexdefs (path, fd, prompts)
  in  currentBasis := readEvalPrintWith eprintln (xdefs, !currentBasis, interactivity)
      before TextIO.closeIn fd
  end 
(* type declarations for consistency checking *)
val _ = op runAs : interactivity -> unit

val asSexp : string -> value = fn s =>
  let val results = finiteStreamOfLine (fn () => NONE) (schemeToken, sexp) s
  in  case listOfStream results
       of [] => SYM ""
        | [v] => v
        | vs => (eprintln ("Values parsed = " ^ String.concatWith ", " (map valueString vs));  embedList vs)
  end

fun setArgv strings =
  currentBasis :=
    processDef ( VAL ("argv", LITERAL (embedList (map asSexp strings)))
               , !currentBasis
               , noninteractive
               )


(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] TO RUN THE INTERPRETER *)
(*                                                               *)
(*****************************************************************)

(* code that looks at command-line arguments and calls [[runAs]] to run the interpreter S214d *)

fun strip_option [] = (NONE, [])
  | strip_option (arg :: args) =
      if arg <> "-" andalso String.isPrefix "-" arg then
          (SOME arg, args)
      else
          (NONE, arg :: args)

val actions =
  [ ("",    fn () => runAs (NOT_PROMPTING, NOT_PRINTING))
  , ("-v",  fn () => runAs (NOT_PROMPTING, PRINTING))
  , ("-vv", fn () => runAs (PROMPTING,     PRINTING))
  , ("-names", fn () => dump_names initialBasis)
  , ("-primitives", fn () => dump_names primitiveBasis)
  , ("-predef", fn () => app println fundefs)
  ]

fun usage () =
  ( app eprint ["Usage:\n"]
  ; app (fn (option, action) =>
         app eprint ["       ", CommandLine.name (), " ", option, "\n"]) actions
  )

fun action option =
  case List.find (curry op = option o fst) actions
    of SOME (_, action) => action ()
     | NONE => usage()

val (myargs, argv) =
  let fun split (mine', []) = (rev mine', [])
        | split (mine', "--" :: argv) = (rev mine', argv)
        | split (mine', s :: ss) = split (s :: mine', ss)
  in  split ([], CommandLine.arguments ())
  end
      
val _ = setArgv argv

val _ = case strip_option myargs
          of (opt, []) => action (getOpt (opt, ""))
           | (NONE, files) => app (runPathAs (NOT_PROMPTING, NOT_PRINTING)) files
           | _      =>   usage ()
