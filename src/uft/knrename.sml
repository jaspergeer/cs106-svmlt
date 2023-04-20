(* In K-normal form, convert string names to register names *)

(* You'll write this file *)

structure KNRename :> sig
  val regOfName : string -> ObjectCode.reg Error.error
  val mapx : ('a -> 'b Error.error) ->
             ('a KNormalForm.exp -> 'b KNormalForm.exp Error.error)
end
  =
struct
  structure K = KNormalForm

  infix 3 <*>   val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>

  val succeed = Error.succeed
  val errorList = Error.list

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun pair x y = (x, y)

  (* AsmLex.registerNum takes a string starting with "r" followed by a number n
     such that 0 <= n < 256, and returns n *)
  val regOfName = AsmLex.registerNum

  fun mapx f = Impossible.exercise "return a function that renames a KNF expression"
end
