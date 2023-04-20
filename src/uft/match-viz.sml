functor MatchViz (structure Tree : DECISION_TREE where type register = int) :> sig
  val viz : ('a -> string) -> 'a Tree.tree -> 'a Tree.tree
    (* returns second argument, and if env variable MATCHVIZ is set,
       writes a visualization to the file of that name *)
end
  =
struct
  structure T = Tree

  val eprint = IOUtil.output TextIO.stdErr

  val outpath = OS.Process.getEnv "MATCHVIZ"
  val outstream = Option.map TextIO.openOut outpath
  val write =
    case outstream of SOME fd => (fn s => TextIO.output (fd, s))
                    | NONE => (fn _ => ())
  val flush : unit -> unit =
    case outstream of SOME fd => (fn _ => TextIO.flushOut fd)
                    | NONE => (fn _ => ())


  local
    val nodesUsed = ref 0
  in
    fun nextNode _ =
      "N" ^ Int.toString (!nodesUsed) before nodesUsed := !nodesUsed + 1
  end

  type attributes = (string * string) list

  fun quote s = String.concat ["\"", s, "\""]
  fun bracket s = String.concat ["[", s, "]"]

  fun keyval (key, v) = String.concat [key, "=", quote v]

  fun render [] = ""
    | render attributes = bracket (String.concatWith "," (map keyval attributes))

  fun label s = [("label", s)] 

  fun node attributes =
    let val name = nextNode ()
        val _ = app write ["  ", name, " ", render attributes, "\n" ]
    in  name
    end

  fun edge from to attributes =
    app write ["  ", from, " -> ", to, " ", render attributes, "\n"]
 
  fun nodeLabeled parts = node (label (String.concat parts))
  fun edgeLabeled from to parts = edge from to (label (String.concat parts))

  val int = Int.toString

  fun con (name, arity) = String.concat [name, "/", int arity]

  fun reg r = "$r" ^ int r

  fun root render nextReg tree =
    let val viz = root render nextReg
    in  case tree
          of T.TEST (r, edges, defaults) =>
               let val root = nodeLabeled ["TEST ", reg r]
                   fun target (T.E (c, t)) = (c, viz t)
                   fun emit (c, to) = edgeLabeled root to [con c]
                   val _ = app (emit o target) edges
                   val _ = case defaults
                             of NONE => ()
                              | SOME t => edgeLabeled root (viz t) ["otherwise"]
               in  root
               end
           | T.MATCH (a, env) =>
               nodeLabeled ["MATCH ", render a, " with ", Env.toString reg env]
           | T.LET_CHILD ((block, i), k) =>
               let val r = nextReg
                   val n = nodeLabeled ["let ", reg r, " = ", reg block, "[", int i, "]"]
                   val child = root render (r + 1) (k r)
                   val () = edge n child []
               in  n
               end
    end

  fun viz render t =
    ( write "digraph {\n"
    ; ignore (root render 100 t)
    ; write "}\n"
    ; flush ()
    ; t
    )

  val maxWidth = 35  (* longest permitted render *)

  fun limit render a =
      let val s = render a
      in  if size s <= maxWidth then
              s
          else
              String.substring (s, 0, maxWidth - 4) ^ " ..."
      end

  val viz = fn render => case outstream
                           of SOME _ => viz (limit render)
                            | NONE => fn t => t
end

                      
