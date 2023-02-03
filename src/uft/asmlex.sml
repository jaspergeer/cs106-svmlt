(* A lexer for assembly language. 
  This is in charge of tokenizing assembly code. *)

(* You'll need to understand what's going on here, and how it's used *)

structure AsmToken = struct
  datatype shape = ROUND | SQUARE | CURLY | ANGLE   (* bracket shape *)

  datatype token = LEFT of shape   (* bracket *)
                 | RIGHT of shape  (* bracket *)
                 | COMMA
                 | COLON
                 | EQUAL
                 | EQUALEQUAL (* == *)
                 | GETS       (* := *)
                 | REGISTER of int
                 | INT      of int
                 | STRING   of string  (* C-style literal with minimal escapes *)
                 | NAME     of string
                 | EOL (* end of line *)
end

structure AsmLex :> sig
  datatype shape = datatype AsmToken.shape (* re-exports type _and constructors_ *)
  datatype token = datatype AsmToken.token
  val unparse : token -> string
  val tokenize : string -> token list Error.error
  val registerNum : string -> int Error.error

  val debug : string -> unit

end
  =    
struct
  datatype shape = datatype AsmToken.shape
  datatype token = datatype AsmToken.token

  val dq = #"\""   (* double quote *)

  fun ceeMinus #"~" = #"-"
    | ceeMinus c    = c

  val brackets = [ (ROUND, "(", ")"), (SQUARE, "[", "]")
                 , (CURLY, "{", "}"), (ANGLE, "\226\159\168", "\226\159\169")
                 ]
    
  infixr 0 $       (* infix function application, from Haskell *)
  fun f $ x = f x

  fun curry f x y = f (x, y)
  fun leftBracket  shape = #2 $ valOf $ List.find (curry op = shape o #1) $ brackets
  fun rightBracket shape = #3 $ valOf $ List.find (curry op = shape o #1) $ brackets

  fun unparse (LEFT shape)  = leftBracket shape
    | unparse (RIGHT shape) = rightBracket shape
    | unparse (COMMA)       = ","
    | unparse (COLON)       = ":"
    | unparse (EQUAL)       = "="
    | unparse (GETS)        = ":="
    | unparse (EQUALEQUAL)  = "=="
    | unparse (REGISTER n)  = "$r" ^ Int.toString n
    | unparse (NAME s)      = s  (* dodgy? *)
    | unparse (STRING s)    = StringEscapes.quote s
    | unparse (INT s)       = String.map ceeMinus (Int.toString s)
    | unparse (EOL)         = "<eol>"

  structure L = MkListProducer (val species = "lexer"
                                type input = char
                                val show = StringEscapes.quote o implode
                               )




  (* wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = L.<*>
  infix 5 <~>      val op <~> = L.<~>
  infixr 4 <$>     val op <$> = L.<$>
  infix 1 <|>      val op <|> = L.<|>
  infix 6 <~> >>   val op <~> = L.<~>  val op >> = L.>>

  val succeed = L.succeed
  val curry = L.curry
  val id = L.id
  val fst = L.fst
  val snd = L.snd
  val many = L.many
  val many1 = L.many1
  val sat = L.sat
  val one = L.one
  val notFollowedBy = L.notFollowedBy
  val eos = L.eos

  type lexer = token L.producer

  fun isDelim c = Char.isSpace c orelse
                  Char.contains "()[]{}:=;," c orelse
                  not (Char.isPrint c)


  fun intFromChars (#"-" :: cs) =
        Error.map Int.~ (intFromChars cs)
    | intFromChars cs =
        (Error.OK o valOf o Int.fromString o implode) cs
        handle Overflow =>
          Error.ERROR "this interpreter can't read arbitrarily large integers"

  val minusSign = sat (L.eq #"-") one

  val intChars =
    (curry (op ::) <$> minusSign <|> succeed id) <*> many1 (sat Char.isDigit one) <~>
    notFollowedBy (sat (not o isDelim) one)

  val intToken = L.check (intFromChars <$> intChars)

  val comment = curry op :: <$> sat (L.eq #";") one <*> many one

  val whitespace =
    curry op @ <$> many (sat Char.isSpace one) <*> (comment <|> succeed [])

  fun strictIntFromString s =
      if List.all Char.isDigit (explode s) then
          Int.fromString s
      else
          NONE

  fun registerNum s =
    let val prefixes = ["r", "$r"]
        fun regNum prefix s =
          if String.isPrefix prefix s then
            (strictIntFromString o String.extract) (s, size prefix, NONE)
          else
            NONE
        fun get (p::ps) s =
              (case regNum p s of SOME n => SOME n | NONE => get ps s)
          | get [] s = NONE
    in  case get prefixes s of
          SOME n => if n >= 0 andalso n < 256 then 
                      Error.succeed n 
                    else 
                      Error.ERROR ("Register number out of range: " ^ Int.toString n)
        | NONE   => Error.ERROR ("Not a register number: " ^ s)
    end

  fun name s =
    case registerNum s
      of Error.OK r => REGISTER r
       | _ => NAME s

  fun char c = sat (curry op = c) one
  fun string s = foldr (fn (c, p) => char c >> p) (L.succeed ()) (explode s)

  fun escape #"n" = Error.OK #"\n"  (* can undo SML Char.toCString *)
    | escape #"t" = Error.OK #"\t"
    | escape #"r" = Error.OK #"\r"
    | escape #"f" = Error.OK #"\f"
    | escape #"v" = Error.OK #"\v"
    | escape #"a" = Error.OK #"\a"
    | escape #"b" = Error.OK #"\b"
    | escape #"\"" = Error.OK #"\""
    | escape #"\\" = Error.OK #"\\"
    | escape #"?" = Error.OK #"?"
    | escape #"'" = Error.OK #"'"
    | escape c = Error.ERROR ("Escape code \\" ^ str c ^ " is undefined")

  fun count 0 p = succeed []
    | count n p = curry op :: <$> p <*> count (n - 1) p

  val digit = sat Char.isDigit one

  fun octal digits =
    case Int.scan StringCvt.OCT (fn (c :: cs) => SOME (c, cs) | [] => NONE) digits
     of SOME (i, []) => SOME (chr i)
      | _ => NONE

  val escapedChar =  char #"\\" >> (  L.maybe octal (count 3 digit)
                                  <|> L.check (escape <$> one)
                                   )
                 <|> sat (curry op <> dq) one
         
  val escapedChar' = L.ofFunction
   (fn s =>
    let val answer = L.asFunction escapedChar s
        val results = case answer
                   of NONE => ["NONE"]
                    | SOME (Error.OK c, ts) => ["char ", str c, " with these left: ", implode ts]
                    | SOME (Error.ERROR s, ts) => ["ERROR ", s, " with these left: ", implode ts]
    in  app print results; print "\n"; answer
    end)
      


  fun bracketParser (shape, left, right) =
        succeed (LEFT  shape) <~> string left
    <|> succeed (RIGHT shape) <~> string right

  val bracketParsers = foldl (fn (x, p) => bracketParser x <|> p) L.pzero brackets

  val token = 
        bracketParsers
    <|> succeed EQUALEQUAL <~> char #"=" <~> char #"="
    <|> succeed EQUAL      <~> char #"="
    <|> succeed GETS       <~> char #":" <~> char #"="
    <|> succeed COLON      <~> char #":"
    <|> succeed COMMA      <~> char #","
    <|> INT <$> intToken
    <|> char dq >> ((STRING o implode) <$> many escapedChar <~> char dq)
    <|> char dq >> L.perror "unterminated quoted string"
    <|> (name o implode) <$> many1 (sat (not o isDelim) one)

  fun manyEOL p = L.fix (fn manyp => curry op :: <$> p <*> manyp <|> succeed [EOL])

  val tokenize = L.produce (whitespace >> manyEOL (token <~> whitespace)) o explode
    : string -> token list Error.error



  fun debug s =
    case tokenize s
      of Error.OK tokens =>
           app print ["SUCCESS: ", String.concatWith " " (map unparse tokens), "\n"]
       | Error.ERROR s => app print ["ERROR: ", s, "\n"]

end
