(* Part of the Pretty Printer *)

(* You can ignore this *)

structure PPTest = struct
  fun readPP' stream =
    let fun get r =
          case TextIO.inputLine stream
            of NONE => PP.LIST (rev r)
             | SOME l => get (PP.Short.$ l :: r)
    in  get []
    end
  fun readPP filename =
    let val s = TextIO.openIn filename
        val pp = readPP' s
    in  TextIO.closeIn s; pp
    end
  val readNormal = PPNormal.normalize o readPP
  fun filter width infile outfile =
    let val normal = readNormal infile
        fun synch (emit, n) = 
          1 before emit(0, ["#line ", Int.toString (n+2), " \"", outfile, "\""]) 
        val out = TextIO.openOut outfile
    in  PPDynamic.set (PPDynamic.standardEmitLine out, synch, width) normal;
        TextIO.closeOut out
    end      
end
