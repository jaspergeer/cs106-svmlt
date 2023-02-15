(* Part of the Pretty Printer *)

(* You can ignore this *)

structure Wppx :>
  sig
    type doc = PP.pretty

    val empty   : doc
    structure Line : sig
                  val optional : doc
                  val connected : doc
                  val forced : doc
              end
   
    val group  : doc -> doc
    val nest   : int -> doc -> doc
    val text   : string -> doc
    val ^^     : doc * doc -> doc

    (* derived functions *)
    val concat : doc list -> doc 
    val seq    : doc -> ('a -> doc) -> 'a list -> doc

    (* Simple converters*)
    val fromConv : ('a -> string) -> 'a -> doc
    val int      : int   -> doc          (* an integer *)
    val char     : char  -> doc          (* an ML character *)
    val word     : word  -> doc          (* an ML word constant *)
    val real     : real  -> doc          (* an ML real constant *)
    val bool     : bool  -> doc          (* a boolean *)


    val toString    : int -> doc -> string 
    val toOutStream : int -> TextIO.outstream -> doc -> unit
    val toFile      : int -> string -> doc -> unit
    val toConsumer  : int -> (string * 'a -> 'a) -> 'a -> doc -> 'a
  end
    =
struct
    structure P = PP.Short
    type doc = PP.pretty
    val empty = P.te ""
    structure Line = struct
                  val optional = P.on
                  val connected = P.cn
                  val forced = P.nl
              end
   
    fun group d = P.li [P.be, d, P.en]
    fun nest i d = P.li [P.i i, d, P.ou]
    val text = P.te
    fun ^^  (d1, d2) = P.li [d1, d2]

    (* derived functions *)

    val concat = P.li
    fun seq sep ppr xs =
        let fun iter nil = []
              | iter [x] = [ppr x]
              | iter (x::xs) = ppr x :: sep :: iter xs
        in  concat (iter xs)
        end

    fun untilde #"~" = #"-"
      | untilde c = c

    fun fromConv conv x = text(conv x)
    val int   = fromConv (String.map untilde o Int.toString)
    val char  = fromConv Char.toString
    val word  = fromConv Word.toString
    val word8 = fromConv Word8.toString
    val real  = fromConv Real.toString
    fun bool b = if b then text "true" else text "false"

    fun synch (emit, n) = 0

  fun toOutStream width out doc =
    let val normal = PPNormal.normalize doc
    in  ignore (PPDynamic.set (PPDynamic.standardEmitLine out, synch, width) normal)
    end      

  fun toString width doc =
    let val normal = PPNormal.normalize doc
        val lines = ref []
        fun add (s, lines) = "\n" :: s :: lines
        val _ = PPDynamic.set (PPDynamic.hoEmit add lines, synch, width) normal
    in  case !lines
         of [] => ""
          | "\n" :: things => String.concat (rev things)
          | things => String.concat (rev things)
    end      

   fun toFile width path doc =
     let val out = TextIO.openOut path
         val _ = toOutStream width out doc
         val _ = TextIO.closeOut out
     in  ()
     end

  fun toConsumer width consume z doc =
    let val normal = PPNormal.normalize doc
        val acc = ref z
        val _ = PPDynamic.set (PPDynamic.hoEmit consume acc, synch, width) normal
    in  !acc
    end

end
