(* Generates abstract assembly code from KNormal-form VScheme *)

(* You'll write this file *)

structure Codegen
  :>
sig 
  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr
  val forEffect : reg KNormalForm.exp -> instruction list
end
  =
struct
  structure A = AsmGen
  structure K = KNormalForm
  structure P = Primitive

  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr

  (********* Join lists, John Hughes (1986) style *********)

  type 'a hughes_list = 'a list -> 'a list
    (* append these lists using `o` *)

  (* don't look at these implementations; look at the types below! *)
  fun empty tail = tail
  fun S e  tail = e :: tail
  fun L es tail = es @ tail

  val _ = empty : 'a hughes_list
  val _ = S     : 'a      -> 'a hughes_list   (* singleton *)
  val _ = L     : 'a list -> 'a hughes_list   (* conversion *)

  (************** the code generator ******************)

  (* three contexts for code generation: to put into a register,
     to run for side effect, or (in module 8) to return. *)

  fun toReg' (dest : reg) (e : reg KNormalForm.exp) : instruction hughes_list =
        Impossible.exercise "toReg"
  and forEffect' (e: reg KNormalForm.exp) : instruction hughes_list  =
        Impossible.exercise "forEffect"
  and toReturn' (e:  reg KNormalForm.exp) : instruction hughes_list  =
        forEffect' e  (* this will change in module 8 *)

  val _ = forEffect' :        reg KNormalForm.exp -> instruction hughes_list
  val _ = toReg'     : reg -> reg KNormalForm.exp -> instruction hughes_list

  fun forEffect e = forEffect' e []

end
