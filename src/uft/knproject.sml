(* Project disambiguated VScheme into KNormal representation. 
    Note that this can fail if the disambiguated VScheme is not already 
    written in KNormal-form. *)

(* You'll complete this file *)

structure KNProject :> sig
  val value : UnambiguousVScheme.value -> KNormalForm.literal
  val def   : UnambiguousVScheme.def -> string KNormalForm.exp Error.error
end 
  = 
struct
  structure K  = KNormalForm
  structure KU = KNormalUtil
  structure P  = Primitive
  structure X  = UnambiguousVScheme

  infix  3 <*>  val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>
  val succeed = Error.succeed
  val error = Error.ERROR
  val errorList = Error.list

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)

  fun checky p = P.name p = "check" orelse P.name p = "expect"

  val asName : X.exp -> X.name Error.error
         (* project into a name; used where KNF expects a name *)
    = fn X.LOCAL x => succeed x 
       | e => error ("expected a local variable but instead got " ^ (X.whatIs e))

  fun value _ = Impossible.exercise "project VScheme value into KNF"
  fun def   _ = Impossible.exercise "project VScheme definition into KNF"

end


