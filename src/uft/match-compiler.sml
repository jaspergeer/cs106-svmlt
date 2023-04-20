signature DECISION_TREE = sig
  (* Describes the target of match compilation, a data structure that
     can be visualized *)
  type register
  type arity = int
  type labeled_constructor = Pattern.vcon * arity
  type pat = Pattern.pat
  datatype 'a tree = TEST      of register * 'a edge list * 'a tree option
                   | LET_CHILD of (register * int) * (register -> 'a tree)
                   | MATCH     of 'a * register Env.env
  and      'a edge = E of labeled_constructor * 'a tree
end

functor MatchCompiler (eqtype register
                       val regString : register -> string)
  : 
sig
  include DECISION_TREE
  val decisionTree : register * (pat * 'a) list -> 'a tree
    (* register argument is the register that will hold
       the value of the scrutinee. *)
end
  =
struct
  structure P = Pattern
  structure LU = ListUtil

  fun fst (x, y) = x
  fun snd (x, y) = y


  (************** BASIC DATA STRUCTURES *************)

  type register = register
  type arity = int
  type labeled_constructor = Pattern.vcon * arity
  type pat = Pattern.pat
  datatype 'a tree = TEST      of register * 'a edge list * 'a tree option
                   | LET_CHILD of (register * int) * (register -> 'a tree)
                   | MATCH     of 'a * register Env.env
  and      'a edge = E of labeled_constructor * 'a tree

  datatype path = REGISTER of register | CHILD of register * int
    (* in order to match block slots, children should be numbered from 1 *)

  type constraint = path * pat
    (* (π, p) is satisfied if the subject subtree at path π matches p *)

  datatype 'a frontier = F of 'a * constraint list
    (* A frontier holds a set of constraints that apply to the scrutinee.

       A choice's initial frontier has just one contraint: [(root, p)],
       where root is the scrutinee register and p is the original pattern
       in the source code.

       A choice is known to match the scrutinee if its frontier
       contains only constraints of the form (REGISTER t, VAR x).
       These constraints show in which register each bound name is stored.
    
       The key operation on frontiers is *refinement* (called `project`
       in the paper).  Refing revises the constraints under the assumption
       that a given register holds an application of a given labeled_constructor 
     *)

  datatype 'a compatibility = INCOMPATIBLE | COMPATIBLE of 'a
   (* Any given point in the decision tree represents knowledge
      about the scrutinee.  At that point, a constraint or a frontier
      may be compatible with that knowledge or incompatible *)

  val mapCompatible : ('a -> 'b compatibility) -> ('a list -> 'b list) =
    (* applies a function to each element of a list; keeps only compatible results *)
    fn f =>
      foldr (fn (a, bs) => case f a of COMPATIBLE b => b :: bs | INCOMPATIBLE => bs) []
           
  val compatibilityConcat : 'a list compatibility list -> 'a list compatibility =
    (* Like List.concat, but result is compatible only if all args are compatible *)
    fn zs =>
      foldr (fn (COMPATIBLE xs, COMPATIBLE ys) => COMPATIBLE (xs @ ys)
              | _ => INCOMPATIBLE)
            (COMPATIBLE [])
            zs



  (************* DEBUGGING SUPPORT ****************)

  val eprint = IOUtil.output TextIO.stdErr

  val patString = WppScheme.patString
  fun pathString (REGISTER r) = regString r
    | pathString (CHILD (r, i)) = regString r ^ "." ^ Int.toString i

  fun frontierString (F (_, constraints)) =
    let fun conString (pi, p) = patString p ^ "@" ^ pathString pi
    in  String.concatWith " /\\ " (map conString constraints)
    end



  (************* DIRTY TRICKS *************)

  (* allow integer literals to masquerade as value constructors *)

  val maybeConstructed : constraint -> (path * P.vcon * pat list) option
    (* maybeConstructed (π, p) 
          = SOME (π, vcon, pats), when p is equivalent to P.APPLY (vcon, pats)
          = NONE                  otherwise
     *)
    = fn (pi, P.APPLY (vcon, pats)) => SOME (pi, vcon, pats)
       | (pi, P.INT k) => SOME (pi, Int.toString k, [])
       | _ => NONE



  (********** USEFUL OPERATIONS ON PATHS AND FRONTIERS ********)

  (* Function `patternAt` implements the @ operation from the paper.
     When `frontier` is `(i, f)`, f@π is `patternAt π frontier` *)

  val patternAt : path -> 'a frontier -> pat option =
    fn pi => fn (F (_, pairs)) =>
      let fun pathIs pi (pi', _) = pi = pi'
      in  Option.map snd (List.find (pathIs pi) pairs)
      end


  (* Substitution for paths: `(new forPath old)` returns
     a substitution function *)

  infix 0 forPath
  fun newPi forPath oldPi =
    let fun constraint (c as (pi, pat)) = if pi = oldPi then (newPi, pat) else c
    in  fn (F (i, constraints)) => F (i, map constraint constraints)
    end


  (************* FUNCTIONS YOU NEED TO WRITE ********)

  (* the match compiler *)

  val refineConstraint :
        register -> labeled_constructor -> constraint -> constraint list compatibility
      (* assuming register r holds C/n,
            refineConstraint r C/n (π, p)
         returns the refinement of constraint (π, p),
         provided p is compatible with C/n at π
       *)
    = fn _ => Impossible.exercise "refineConstraint (in lab)"

  val refineFrontier :
        register -> labeled_constructor -> 'a frontier -> 'a frontier option
      (* returns the refinement of the given frontier, if compatible
       *)
    = fn _ => Impossible.exercise "refineFrontier"

  fun decisionTree _ =
    Impossible.exercise "decisionTree"


end




