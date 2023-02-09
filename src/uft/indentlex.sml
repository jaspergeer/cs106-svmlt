(* A lexer to handle Pythonic indentation *)

structure IndentLex :> sig
  datatype 'token plus_indent
    = INDENT
    | OUTDENT
    | NEWLINE
    | TOKEN of 'token

  val tokenize : (string -> 'token list Error.error) -> 
                 string list -> 'token plus_indent list Error.error
  val unparse : ('t -> string) -> 't plus_indent -> string
end
  =
struct
  datatype 'token plus_indent
    = INDENT
    | OUTDENT
    | NEWLINE
    | TOKEN of 'token

  structure E = Error

  exception BadSpace of char (* leading space other than ASCII 32 *)
       
  val removeIndent : string -> int * string =
    (* Remove prefix spaces from input, return amount of
       indentation plus remaining characters.
       Error if the prefix contains a space character
       other than ASCII 32 *)
    let fun takeSpace (n, []) = (n, "")
          | takeSpace (n, c :: cs) =
              if c = #" " then
                  takeSpace (n + 1, cs)
              else if Char.isSpace c then
                  raise BadSpace c
              else
                  (n, implode (c :: cs))
    in  fn s => takeSpace (0, explode s)
    end

  exception BadIndent of int * int list * int
     (* current indent, prior indents, the bad indent *)

  val indentTokens : int * int list -> int -> (int * int list) * 'a plus_indent list
    = fn (current, prior) => fn indent =>
        case Int.compare (indent, current)
          of EQUAL => ((current, prior), [])  (* no change *)
           | GREATER => ((indent, current :: prior), [INDENT])
           | LESS =>
               let fun down [] 0 tokens = ((0, []), tokens)
                     | down [] i tokens = raise BadIndent (current, prior, indent)
                     | down (c::p) i tokens =
                         case Int.compare (i, c)
                          of GREATER => raise BadIndent (current, prior, indent)
                           | EQUAL => ((i, p), tokens)
                           | LESS => down p i (OUTDENT :: tokens)
               in  down prior indent [OUTDENT]
               end

  fun eofTokens (0, []) = []
    | eofTokens (0, _ :: _) = Impossible.impossible "zero indentation with prior"
    | eofTokens (i, []) = Impossible.impossible "nonzero indentation with no prior"
    | eofTokens (i, c :: p) = OUTDENT :: eofTokens (c, p)

  exception Err of string
  fun unError (E.OK a) = a
    | unError (E.ERROR msg) = raise Err msg

  exception WithLine of exn * string

  val int = Int.toString

  fun exnMessage (WithLine (e as WithLine _, _)) = exnMessage e
    | exnMessage (WithLine (e, s)) = "In line `" ^ s ^ "`, " ^ exnMessage e
    | exnMessage (BadSpace c) = "Bad space character '" ^ Char.toString c ^ "'"
    | exnMessage (BadIndent (rightmost, other, i)) =
        let fun commas [] = ["huh?"]
              | commas [i] = [", or ", int i]
              | commas (i :: is) = ", " :: int i :: commas is
        in  String.concat (["Found indentation ", int i, " when the only permissible ",
                            "indentations were ", int rightmost, " or greater"] @
                           commas other)
        end
    | exnMessage (Err e) = e
    | exnMessage e = raise e

  fun doLine theTokens context (line :: lines) =
       (let val (i, line) = removeIndent line
            val (context, tokens) = indentTokens context i
        in  tokens :: map TOKEN (unError (theTokens line)) :: [NEWLINE] ::
            doLine theTokens context lines
        end handle e => raise (WithLine (e, line)))
    | doLine _ context [] = [eofTokens context ]

  fun tokenize theTokens lines =
      E.OK (List.concat (doLine theTokens (0, []) lines))
      handle e => E.ERROR (exnMessage e)

  fun unparse u =
    fn INDENT => "\226\135\168"  (* unicode right arrow *)
     | OUTDENT => "\226\135\166"  (* unicode left arrow *)
     | NEWLINE => "\\n"
     | TOKEN t => u t

end       

structure ILTest = struct
  val good1 =
      [ "first"
      , "  second"
      , "third"
      ]

  val good2 =
      [ "first"
      , "  second"
      , "    third"
      , "fourth"
      ]

  val bad1 = 
      [ "first"
      , "  second"
      , " third"
      , "fourth"
      ]

  val tokenize = IndentLex.tokenize AsmLex.tokenize
  val unparse = IndentLex.unparse AsmLex.unparse

  fun show lines = 
    IOUtil.outln TextIO.stdErr (
    case tokenize lines
     of Error.OK ts => 
        "tokens: " ^ String.concatWith "\194\183" (map unparse ts)
      | Error.ERROR msg => "error: " ^ msg)

  fun assert what p = if p then () else Impossible.impossible what

  local
    val myt = tokenize
    open AsmLex
    open IndentLex
    val t = TOKEN
  in
    val _ = assert "good1 fails"
      (myt good1 = Error.OK [ t (NAME "first"), t EOL, NEWLINE
                            , INDENT, t (NAME "second"), t EOL, NEWLINE
                            , OUTDENT, t (NAME "third"), t EOL, NEWLINE
                            ])
    val _ = assert "good2 fails"
      (myt good2 = Error.OK [ t (NAME "first"), t EOL, NEWLINE
                            , INDENT, t (NAME "second"), t EOL, NEWLINE
                            , INDENT, t (NAME "third"), t EOL, NEWLINE
                            , OUTDENT, OUTDENT, t (NAME "fourth"), t EOL, NEWLINE
                            ])

    val _ = case myt bad1 of Error.ERROR _ => ()
                           | Error.OK _ => Impossible.impossible "bad1 lexes"
  end
end
