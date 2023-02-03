(* This is the universal forward translator. As you build the different VScheme 
    representations and the translations between them, you'll chain together 
    these translations here. It implements the actual translations *)

(* You'll get a partially complete version of this file, 
    which you'll need to complete *)

structure UFT :> sig
  type language = Languages.language
  exception NotForward of language * language
  val translate : language * language -> TextIO.instream * TextIO.outstream -> unit Error.error
end
  =
struct

  (**** I/O functions and types ****)

  type instream = TextIO.instream
  val lines  = IOUtil.lines : instream -> string list
  val output = IOUtil.output 
  val outln  = IOUtil.outln


  (**** function composition, including errors ****)

  type 'a error = 'a Error.error

  infix 0 >>> >=>
  fun f >>> g = fn x => g (f x)         (* function composition, Elm style *)
  val op >=> = Error.>=>

  fun liftMap f xs = Error.list (map f xs)  (* liftMap f == map f >>> Error.list *)


  (**** Reader functions ****)

  val VS_of_file : instream -> AssemblyCode.instr list error =
    lines                    (* line list *)
    >>> map AsmLex.tokenize  (* token list error list *)
    >>> Error.list           (* token list list error *)
    >=> AsmParse.parse       (* instr list error *)    


  (**** Support for materialization ****)
  
  exception Backward  (* for internal use only *)
                      (* raised if someone commands a backward translation *)

  datatype language = datatype Languages.language (* imports value constructors *)
  exception NoTranslationTo of language  (* used internally *)

  val ! = Error.map  (* useful abbreviation for materializers and `translate` *)

  (**** Materializer functions ****)
  
  fun VS_of VS   = VS_of_file
    | VS_of inLang = raise NoTranslationTo VS

  fun VO_of VO     = (fn _ => Error.ERROR "There is no reader for .vo")
    | VO_of inLang = VS_of inLang >=> Assembler.translate


  (**** Emitter functions ****)

  fun emitVO outfile = app (outln outfile) o ObjectUnparser.module
  fun emitVS outfile = app (outln outfile) o AsmParse.unparse


  (**** The Universal Forward Translator ****)

  exception NotForward of language * language  (* for external consumption *)

  fun translate (inLang, outLang) (infile, outfile) =
    (case outLang
       of VO => VO_of      inLang >>> ! (emitVO outfile)
        | VS => VS_of      inLang >>> ! (emitVS outfile)
        | _  => raise NoTranslationTo outLang
    ) infile
    handle Backward => raise NotForward (inLang, outLang)
         | NoTranslationTo outLang => raise NotForward (inLang, outLang)
end
