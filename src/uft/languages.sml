(* All the languages we'll eventually translate, 
    and the order in which they can be translated *)

(* You'll need to use the signature, 
    but don't need to look at the implementation *)

structure Languages :> sig
  datatype language = ES | HOX | HO | FO | CL | KN | VS | VO
  val table : { language : language, short : string, description : string } list

  val find : string -> language option
  val description : language -> string
  val le : language * language -> bool   (* determines forward *)
end
  =
struct
  datatype language = ES | HOX | HO | FO | CL | KN | VS | VO

  fun inject (l, s, d) = { language = l, short = s, description = d }

  val table = map inject
    [ (ES,  "es",  "eScheme (ho vScheme plus Erlang-style pattern matching)")
    , (HOX, "hox", "Higher-order vScheme with mutable variables in closures")
    , (HO,  "ho",  "Higher-order vScheme")
    , (FO,  "fo",  "First-order vScheme")
    , (CL,  "cl",  "First-order vScheme with closure and capture forms")
    , (KN,  "kn",  "K-Normal form")
    , (VS,  "vs",  "VM assembly language")
    , (VO,  "vo",  "VM object code")
    ]


  fun find x = Option.map #language (List.find (fn r => #short r = x) table)

  fun entry lang = Option.valOf (List.find (fn r => #language r = lang) table)

  val description = #description o entry

  fun pred VO  = SOME VS
    | pred VS  = SOME KN
    | pred KN  = SOME CL
    | pred CL  = SOME FO
    | pred FO  = SOME HO
    | pred HO  = SOME HOX
    | pred HOX = NONE
    | pred ES  = NONE

  fun le (ES, _) = true
    | le (from, to) =
        from = to orelse (case pred to of SOME x => le (from, x) | NONE => false)
end
