(* Representation of KNormal-form, with some utility functions *)

(* You'll define the representation of `'a exp`, and you'll write
   the utility functions. *)

structure KNormalForm = struct
  
  datatype literal = datatype ObjectCode.literal

  type vmop = Primitive.primitive

  datatype 'a exp      (* type parameter 'a is a _name_, typically
                          instantiated as `string` or `ObjectCode.reg` *)
    = LEFT_AS_EXERCISE
end

structure KNormalUtil :> sig
  type name = string
  val setglobal : name * 'a -> 'a KNormalForm.exp
  val getglobal : name -> 'a KNormalForm.exp

   (* create these @(x,...x, v) forms:
         setglobal(register, name-of-global)
         getglobal(name-of-global)
    *)

  (* you could consider adding similar functions for `check`, `expect`,
     and `check-assert` *)
end
  =
struct
  structure K = KNormalForm
  type name = string

  fun setglobal (x, register) = Impossible.exercise "setglobal"
  fun getglobal x             = Impossible.exercise "getglobal"

end
