(* A parser for assembly language *)

(* You'll get a partially complete version of this file, 
  which you'll need to complete. *)

structure AsmParse :>
  sig
    type line = string (* one line of assembly code *)
    val parse   : AsmLex.token list  list -> AssemblyCode.instr list Error.error
    val unparse : AssemblyCode.instr list -> line list
  end
  =
struct
  (* visualize list of tokens using Unicode middle dot as separator *)
  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map AsmLex.unparse ts) ^ "]"

  structure P = MkListProducer (val species = "parser"
                                type input = AsmLex.token
                                val show = showTokens
                               )
      (* P for parser; builds a module that takes AsmLex.token as input *)

  structure L = AsmLex
  structure A = AssemblyCode
  structure O = ObjectCode

  type line = string (* one line of assembly code *)

  (* Operations on producers: Wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = P.<*>
  infixr 4 <$>     val op <$> = P.<$>
  infix 3 <~>      val op <~> = P.<~>
  infix 1 <|>      val op <|> = P.<|>
  infix 3 >>        val op >> = P.>>

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
  fun flip f x y = f y x

  (* utilities *)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  type 'a parser = 'a P.producer

  (* always-error parser; useful for messages *)
  fun expected what =
    let fun bads ts = Error.ERROR ("looking for " ^ what ^
                                   ", got this input: " ^ showTokens ts)
    in  P.check ( bads <$> many one )
    end

  (* always-succeed parser; prints msg when run *)
  fun debug msg =
      P.ofFunction (fn ts => (app eprint ["@> ", msg, "\n"]; SOME (Error.OK (), ts)))

  (* make another parser chatter on entry *)
  val verbose : string -> 'a parser -> 'a parser
   = (fn msg => fn p => debug msg >> p)

  val veryVerbose : string -> 'a parser -> 'a parser
      = (fn what => fn p =>
           let fun shout s = app eprint ["looking for ", what, s, "\n"]
           in  P.ofFunction (fn ts =>
                                let val _ = shout "..."
                                    val answer = P.asFunction p ts
                                    val _ =
                                        case answer
                                          of NONE => shout ": failed"
                                           | SOME (Error.ERROR _, _) => shout ": errored"
                                           | SOME (Error.OK _, _) => shout ": succeeded"
                                in  answer
                                end)
           end)

  (****************************************************************************)

  (**** parsers for common tokens ****)

      (* These are your workhorse parsers---the analog of the `get`
         functions from the `tokens.h` interface in the SVM *)

  val int       = P.maybe (fn (L.INT   n)    => SOME n  | _ => NONE) one
  val name      = P.maybe (fn (L.NAME  n)    => SOME n  | _ => NONE) one
  val string    = P.maybe (fn (L.STRING s)   => SOME s  | _ => NONE) one
  val reg       = P.maybe (fn (L.REGISTER n) => SOME n  | _ => NONE) one

  fun token t = sat (P.eq t) one >> succeed () (* parse any token *)
  val eol = token L.EOL


  (* turn any single-token string into a parser for that token *)
  fun the "\n" = eol
    | the s =
        case AsmLex.tokenize s
          of Error.OK [t, AsmLex.EOL] => sat (P.eq t) one >> succeed ()
           | _ => (app eprint ["fail: `", s, "`\n"]; Impossible.impossible "non-token in assembler parser")

  (* fun commaSep p = many (p <~> the ",") <|> succeed[] *)
  fun commaSep p = (curry (op ::) <$> p <*> many (the "," >> p)) <|> succeed []
  
  val _ = op commaSep : 'a parser -> 'a list parser

  val literal
    = (O.INT <$> int)
    <|> (O.STRING <$> string)
    <|> (fn _ => O.NIL) <$> the "nil"
    <|> (fn _ => O.EMPTYLIST) <$> the "emptylist"
    <|> (fn _ => O.BOOL true) <$> the "true"
    <|> (fn _ => O.BOOL false) <$> the "false"


  (***** instruction-building functions for parsers ****)

  fun regs operator operands = A.OBJECT_CODE (O.REGS (operator, operands))
     (* curried instruction builder *)

  fun eR0 operator          = regs operator []
  fun eR1 operator r1       = regs operator [r1]
  fun eR2 operator r1 r2    = regs operator [r1, r2]
  fun eR3 operator r1 r2 r3 = regs operator [r1, r2, r3]

  fun eRLIT operator r1 lit = A.OBJECT_CODE (O.REGSLIT (operator, [r1], lit))

  (***** Example parser for you to extend *****)

  (* The example parser includes "passthrough" syntax and three
     demonstration instructions:

        - Add two registers, put result in a third

        - Add a small immediate constaint to a register,
          or subtract a small immediate constant from a register,
          put result in a third.

        - Swap the contents of two registers.

     The latter two demonstrations show some more sophisticated
     parsing techniques.
   *)
                      

  (* Swap uses standard multiple-assignment syntax:  $r9, $r33 := $r33, $r9
     Helper function `swap` ensures that register numbers't match.
   *)
  fun swap r1 r2 r3 r4 =
    if r1 = r4 andalso r2 = r3 then
        Error.OK (eR2 "swap" r1 r2)
    else
        Error.ERROR "multiple assignment is allowed only for register swaps"

  (* The add-immediate instruction uses "offset coding": a byte
     in the range 0..255 is converted to signed by subtracting 128.
   *)
  val offset_code : int -> int Error.error =
    fn n => if n >= ~128 andalso n < 128 then Error.OK (n + 128)
            else Error.ERROR ("small integer " ^ Int.toString n ^
                              " out of range -128..127")

  (* To use offset coding, I define a version of <$> that checks for errors *)
  infixr 4 <$>!
  fun f <$>! p = P.check (f <$> p)

  (* Example parser: reads an instruction /without/ reading end of line *)

  val one_line_instr : A.instr parser
     =  the "@" >> regs <$> name <*> many int  (* "passthrough" syntax *)
    <|> eR3 "+"    <$> reg <~> the ":=" <*> reg <~> the "+" <*> reg
    <|> eR3 "+imm" <$> reg <~> the ":=" <*> reg <~> the "+" <*> (offset_code <$>! int)
    <|> eR3 "+imm" <$> reg <~> the ":=" <*>
                       reg <~> the "-" <*> ((offset_code o ~) <$>! int)
                                           (* the ~ is ML's unary minus (negation) *)
    <|> P.check
        (swap <$> reg <~> the "," <*> reg <~> the ":=" <*> reg <~> the "," <*> reg)
    (* You add cases here.  Put cases with longer inputs first, e.g.,
          r1 := r2 + r3
       before
          r1 := r3
     *)    
    <|> eRLIT "loadliteral" <$> reg <~> the ":=" <*> literal


   (**** recursive parser that handles end-of-line and function loading ****)

   (* Parsers for start and end of "load function", for you to write.
      Designing syntax so each one terminates with `eol` is recommended. *)

   fun loadfunc (reg, arity) body = A.LOADFUNC (reg, arity, body)
   val loadfunStart : (int * int) parser = (* fill in with (reg * arity) parser *)
         P.pzero <~> eol
   val loadfunEnd : unit parser =
         P.pzero <~> eol

   (* grammar :   <instruction> ::= <one_line_instruction> EOL
                                 | <loadfunStart> {<instruction>} <loadfunEnd> *)

   (* simple parser with no error detection *)
   val instruction : A.instr Error.error parser
     = Error.OK <$>
       P.fix (fn instruction : A.instr parser =>
                   one_line_instr <~> many1 eol
               <|> loadfunc <$> loadfunStart <*> many instruction <~> loadfunEnd)

   (* A better parser is juiced up with extra error detection *)

   fun badTokens ts = Error.ERROR ("unrecognized assembly line: " ^ showTokens ts)
   val nonEOL = sat (curry op <> L.EOL) one  (* any token except EOL *)

   val instruction : A.instr Error.error parser
     = P.fix
       (fn instruction : A.instr Error.error parser =>
              Error.OK <$> one_line_instr <~> many1 eol
          <|> Error.OK <$>
              (loadfunc <$> loadfunStart <*> 
                            P.check (Error.list <$> many instruction) <~>
                            loadfunEnd)
          <|> P.notFollowedBy loadfunEnd >>
              (* gobble to end of line, then succeed by producing error message: *)
              badTokens <$> many nonEOL <~> eol  
       )


  val parse = Error.join o P.produce (Error.list <$> (many eol >> many instruction)) o List.concat
            

  (*************************** unparsing *****************************)

  val int = Int.toString
  fun reg r = "$r" ^ int r
  val spaceSep = String.concatWith " "
  val lit_toString = ObjectUnparser.literal

  fun unparse1 (A.OBJECT_CODE (O.REGS ("+", [x, y, z]))) =
        spaceSep [reg x, ":=", reg y, "+", reg z]
    | unparse1 (A.OBJECT_CODE (O.REGS ("+imm", [x, y, z]))) =
        spaceSep [reg x, ":=", reg y, "+", int z]
    | unparse1 (A.OBJECT_CODE (O.REGSLIT ("loadliteral", x, y))) =
        spaceSep ((map reg x) @ [":="] @ lit_toString y)
        (* unspecified eR3 should be prefixed *)
    | unparse1 (A.OBJECT_CODE (O.REGS (opcode, [x, y, z]))) =
        spaceSep [opcode, reg x, reg y, reg z]
        (* same as eR2, eR1, eR*)
    | unparse1 (A.OBJECT_CODE (O.REGS (opcode, [x, y]))) =
        spaceSep [opcode, reg x, reg y]
    | unparse1 (A.OBJECT_CODE (O.REGS (opcode, [x]))) =
        spaceSep [opcode, reg x]
    | unparse1 (A.OBJECT_CODE (O.REGS (opcode, []))) =
        opcode
    | unparse1 (A.DEFLABEL label) = spaceSep ["def", label]
    | unparse1 (A.GOTO_LABEL label) = spaceSep ["goto", label]
    | unparse1 (A.IF_GOTO_LABEL (x, label)) =
      spaceSep ["if", reg x, "goto", label]
    | unparse1 _ = "an unknown assembly-code instruction"

  
  (* val unparse : AssemblyCode.instr list -> string list *)
  fun unparse [] = []
    | unparse (i::is) = (case i of
                              A.LOADFUNC (x, arity, body) =>
                                  List.concat [".loadfunc" :: (unparse body), 
                                                ".endload" :: (unparse is)]
                            | _ => (unparse1 i) :: (unparse is))
end
