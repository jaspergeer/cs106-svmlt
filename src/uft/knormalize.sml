(* KNormalizer from First-Order Scheme to KNormal-form Scheme. 
    This is where register allocation happens! *)

(* You'll fill out the missing code in this file *)

structure KNormalize :> sig
  type reg = int  (* register *)
  type regset     (* set of registers *)
  val regname : reg -> string
  val exp : reg Env.env -> regset -> FirstOrderScheme.exp -> reg KNormalForm.exp
  val def :                          FirstOrderScheme.def -> reg KNormalForm.exp
end 
  =
struct 
  structure K  = KNormalForm
  structure F  = FirstOrderScheme
  structure E  = Env
  structure P  = Primitive

  structure MC = MatchCompiler(type register = int
                               fun regString r = "$r" ^ Int.toString r
                              )

  structure MV = MatchViz(structure Tree = MC)
  val vizTree = MV.viz (WppScheme.expString o CSUtil.embedExp)


  infix 6 <+>
  val op <+> = Env.<+>

  fun fst (x, y) = x
  fun member x = List.exists (fn y => x = y)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  (************ Register and regset operations ************)

  type reg = int
  fun regname r = "$r" ^ Int.toString r

  datatype regset = RS of int (* RS n represents { r | r >= n } *)

  (************ K-normalization ************)

  type exp = reg K.exp
  type policy = regset -> exp -> (reg -> exp) -> exp
    (* puts the expression in an register, continues *)

  type 'a normalizer = regset -> 'a -> exp

  fun nbRegsWith normalize bind A xs k =
        Impossible.exercise "nbRegsWith, to be implemented in a later step"

  val nbRegsWith : 'a normalizer -> policy -> regset -> 'a list -> (reg list -> exp) -> exp
    = nbRegsWith

  fun exp rho A e =
    let val exp : reg Env.env -> regset -> FirstOrderScheme.exp -> exp = exp
        val nbRegs = nbRegsWith (exp rho)   (* normalize and bind in _this_ environment *)
    in  case e
          of _ => Impossible.exercise "K-normalize an expression"
    end

  fun def e = Impossible.exercise "K-normalize a definition"

end
