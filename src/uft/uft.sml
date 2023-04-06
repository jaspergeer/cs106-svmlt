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

  val schemeOfFile : instream -> VScheme.def list error =
    lines                                   (* line list *)
    >>>  SxParse.parse                      (* sx list error *)
    >=>  Error.mapList VSchemeParsers.defs  (* def list list error *)
    >>>  Error.map List.concat              (* def list error *)
    >>>  Error.map VSchemeTests.delay
    
  val schemexOfFile : instream -> UnambiguousVScheme.def list error =
    schemeOfFile >>>
    Error.map (map Disambiguate.disambiguate)

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
  
  fun HOX_of HOX  = schemexOfFile
    | HOX_of _    = raise Backward

  fun HO_of HO   = schemexOfFile
    | HO_of HOX  = Impossible.unimp "imperative features (HOX to HO)"
    | HO_of _    = raise Backward

  fun CL_of CL     = CL_of FO   (* really *)
    | CL_of HO     = HO_of HO     >>> ! (map ClosureConvert.close)
    | CL_of HOX    = HO_of HOX    >>> ! (map ClosureConvert.close)
    | CL_of inLang = FO_of inLang >>> ! (map FOCLUtil.embed)

  fun VS_of VS   = VS_of_file
    | VS_of inLang = raise NoTranslationTo VS

  fun VO_of VO     = (fn _ => Error.ERROR "There is no reader for .vo")
    | VO_of inLang = VS_of inLang >=> Assembler.translate


  (**** Emitter functions ****)

  val width =  (* parameter to prettyprinter *)
    case Option.mapPartial Int.fromString (OS.Process.getEnv "WIDTH")
      of SOME n => n
       | NONE => 72

  fun emitVO outfile = app (outln outfile) o ObjectUnparser.module
  fun emitVS outfile = app (outln outfile) o AsmParse.unparse

  fun emitScheme outfile = Wppx.toOutStream width outfile o WppScheme.pp

  fun emitHO outfile = app (emitScheme outfile o Disambiguate.ambiguate)


  (**** The Universal Forward Translator ****)

  exception NotForward of language * language  (* for external consumption *)

  fun translate (inLang, outLang) (infile, outfile) =
    (case outLang
       of VO => VO_of      inLang >>> ! (emitVO outfile)
        | VS => VS_of      inLang >>> ! (emitVS outfile)
        | HO => HO_of      inLang >>> ! (emitHO outfile)
        | _  => raise NoTranslationTo outLang
    ) infile
    handle Backward => raise NotForward (inLang, outLang)
         | NoTranslationTo outLang => raise NotForward (inLang, outLang)
end
