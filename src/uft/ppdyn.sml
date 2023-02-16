(* Part of the Pretty Printer *)

(* You can ignore this *)

structure NewlinePPCost = struct
  type cost = int
  val initialCost = 0
  val op < = op < : cost * cost -> bool
  fun addNewline (cost, depth, revline) = cost + 1
end
structure NewlineDepthPPCost = struct
  type cost = int * { depth : int, count : int } list 
  local
    fun count depths depth = 
      let fun cnt [] = 0
            | cnt ({depth=d, count=n}::t) = case Int.compare (depth, d) 
                                              of LESS => cnt t
                                               | EQUAL => n
                                               | GREATER => 0
      in  cnt depths
      end
    fun maxDepth [] = 0
      | maxDepth ({depth, count}::t) = depth
    (* add a newline at depth *)
    fun addAtDepth ([], depth') = [{depth=depth', count=1}]
      | addAtDepth(depths as ({depth, count}::t), depth') =
          case Int.compare (depth, depth')
            of LESS => {depth=depth', count=1} :: depths
             | EQUAL => {depth=depth, count=count+1} :: t
             | GREATER => {depth=depth,count=count} :: addAtDepth(t, depth')
    fun lt ((newlines, depths), (newlines', depths')) = 
      case Int.compare (newlines, newlines')
        of LESS => true
         | GREATER => false
         | EQUAL =>
             let val c = count depths
                 val c' = count depths'
                 fun cmp depth = Int.compare(c depth, c' depth)
                 fun lt 0 = cmp 0 = LESS
                   | lt n = case cmp n of LESS => true
                                        | EQUAL => lt (n-1)
                                        | GREATER => false
             in  lt (Int.max (maxDepth depths, maxDepth depths'))
             end
  in
    val initialCost : cost = (0, [])
    fun addNewline ((newlines, depths), depth, revline) =
          (newlines+1, addAtDepth(depths, depth))
    val op < = lt   
  end
end
functor PPDynamicFun(Cost:PP_COST) : PP_DYNAMIC = struct
  type emitter = int * string list -> unit
  type syncher = emitter * int -> int
  type cost = Cost.cost
  datatype feasible = INITIAL
                    | PREV of partial
  withtype partial = { revline : string list  (* contents of the line *)
                     , indent : int           (* amount to indent this line *)
                     , remaining : int        (* width - indent - size of revline *)
                     , prev : feasible        (* chain of all previous breaks *)
                     , cost : cost            (* cost of breaking at prev *)
                     , synch : bool           (* #line preceding this line *)
                     }
  local
    fun addText "" x = x
      | addText s {revline, cost, remaining, prev, indent, synch} =
      {revline = s::revline, cost = cost, remaining = remaining - size s , prev = prev,
       indent=indent, synch=synch}
    fun betterOverfull(a, b : partial) = if #remaining a > #remaining b then a else b
    fun betterCost(a, b : partial) = if Cost.<(#cost a, #cost b) then a else b
    fun findBest better (h::t) = foldl better h t
      | findBest _ [] = Impossible.impossible "candidate invariant violated"
    fun removeOverfull (candidates : partial list) =
        let val c = List.filter (fn c => #remaining c >= 0) candidates
        in  if null c then [findBest betterOverfull candidates] else c
        end
    structure N = PPNormal
    exception CannotSetSolid
    fun setSolid (l, remaining) =
      let fun text (s, (r, revline)) =
                let val r = r - size s
                in  if r < 0 then raise CannotSetSolid else (r, s :: revline)
                end
          and addString(N.TEXT s, (r, revline)) = text(s, (r, revline))
            | addString(N.BREAK (_, {break=PP.FORCED, ...}), _) = raise CannotSetSolid
            | addString(N.BREAK (_, {none,...}), (r, revline)) = text(none, (r, revline))
            | addString(N.SYNCH _, _) = raise CannotSetSolid
            | addString(N.BLOCK b, (r, l)) = addString(N.TEXT (setSolid (b, r)), (r, l))
          val (_, revline) = foldl addString (remaining, []) l
      in  String.concat (rev revline)
      end
  in
    fun set (emitLine, emitSynch, width) pretty =
      let (* invariant: candidates is never empty *)
	  local
	    fun newCandidates "" candidates = candidates
	      | newCandidates s candidates = removeOverfull (map (addText s) candidates)
	    fun newPartial (best, depth, synch, i) : partial =
	          { cost = Cost.addNewline (#cost best, depth, #revline best), synch = synch,
	            indent = i, revline = [], remaining = width - i, prev = PREV best }
	    fun append(N.TEXT s :: tail, depth, candidates) = 
	          append(tail, depth, newCandidates s candidates)
	      | append(N.BREAK (i, {break, pre, post, none}) :: tail, depth, candidates) =
	          let val forced = case break of PP.CONNECTED => true
	                                       | PP.FORCED => true
	                                       | PP.OPTIONAL => false
	              val best = findBest betterCost (newCandidates pre candidates)
	              val new = addText post (newPartial (best, depth, false, i))
	              val candidates = newCandidates none candidates
	          in  append(tail, depth, if forced then [new] else new :: candidates)
	          end
	      | append(N.SYNCH i :: tail, depth, candidates) =
	          let val best = findBest betterCost candidates
	              val new = newPartial (best, depth, true, i)
	          in  append(tail, depth, [new])
	          end
	      | append(N.BLOCK b :: tail, depth, candidates) =
	          let fun isBreak category (N.BREAK(_, {break=cat', ...})) = category = cat'
		        | isBreak _ _ = false
		      val hasForced    = List.exists (isBreak PP.FORCED)    b
		      val hasConnected = List.exists (isBreak PP.CONNECTED) b
		      val candidates =
		        if hasForced orelse (not hasForced andalso not hasConnected) then
		          append(b, depth+1, candidates)
		        else
		          let val openCandidates = append(b, depth+1, candidates)
		              val maxRemaining = foldl (fn(c, r) => Int.max (#remaining c, r))
		                                       0 candidates
		          in  let val s = setSolid (b, maxRemaining)
		              in  removeOverfull(
		                    foldl (fn (c, cs) => addText s c :: cs) openCandidates candidates)
		              end handle CannotSetSolid => openCandidates
		          end
		  in  append(tail, depth, candidates)
		  end
	      | append([], _, candidates) = candidates
	  in 
	    val append = append
	  end
          val init : partial = {revline=[], cost=Cost.initialCost, synch=false,
                                remaining=width, indent=0, prev=INITIAL}
          val answer = findBest betterCost (append([pretty], 0, [init]))
          fun emit (PREV {revline, prev, indent, synch, ...}) =
                let val n = emit prev
                    val s = if synch then emitSynch (emitLine, n) else 0
                in  ( emitLine (indent, rev revline)
                    ; n + s + 1
                    )
                end
            | emit INITIAL = 0
      in  emit (PREV answer)
      end
    fun hoEmit consume acc (n, ss) =
      let fun extend 0 ss = ss
            | extend n ss = extend (n - 1) (" " :: ss)
          val line = if n >= 0 then extend n ss
                     else "((pp error: negative indent))" :: ss
      in  acc := consume (String.concat ss, !acc)
      end

    fun standardEmitLine stream (n, ss) =
      let fun puts s = TextIO.output(stream, s)
          fun emit(0, ss) = (List.app puts ss; puts "\n")
            | emit(n, ss) = (puts " "; emit(n-1, ss))
      in  if n >= 0 then emit(n, ss)
          else (puts "((pp error: negative indent))"; emit(0, ss))
      end
  end
end

structure PPDynamic' = PPDynamicFun(NewlinePPCost)
structure PPDynamic = PPDynamicFun(NewlineDepthPPCost)
