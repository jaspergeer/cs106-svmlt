(* Producer interface [parsing combinators], developed in module 3 lab *)

(* You'll get a partially complete version of this file,
    which you'll need to complete *)

signature PRODUCER = sig
  type input
  type 'a error = 'a Error.error
  type 'a producer
  type 'a producer_fun = input list -> ('a error * input list) option

  val asFunction : 'a producer     -> 'a producer_fun
  val ofFunction : 'a producer_fun -> 'a producer

  val produce : 'a producer -> input list -> 'a error
    (* consumes the entire list to produce a single 'a, or errors *)


  (* main builders: applicative functor plus alternative *)
  val succeed : 'a -> 'a producer
  val <*> : ('a -> 'b) producer * 'a producer -> 'b producer
  val <$> : ('a -> 'b) * 'a producer -> 'b producer
  val <|> : 'a producer * 'a producer -> 'a producer

  (* shortcuts for parsing something and dropping the result *)
  val <~> : 'a producer * 'b producer -> 'a producer
  val >>  : 'a producer * 'b producer -> 'b producer

  (* conditional parsing *)
  val sat   : ('a -> bool)      -> 'a producer -> 'a producer
  val maybe : ('a -> 'b option) -> 'a producer -> 'b producer

  val eos : unit producer   (* end of stream *)
  val one : input producer  (* one token *)

  (* classic EBNF, plus "one or more" *)
  val optional : 'a producer -> 'a option producer
  val many  : 'a producer -> 'a list producer
  val many1 : 'a producer -> 'a list producer


  (* check for a semantic error, turn it into a syntax error *)
  val check : 'a error producer -> 'a producer


  (* occasionally useful *)
  val pzero : 'a producer (* always fails *)
  val perror : string -> 'a producer (* always errors *)

  (* for special things *)
  val notFollowedBy : 'a producer -> unit producer

  (* recursive parsers *)
  val fix : ('a producer -> 'a producer) -> 'a producer   (* for usage see below *)
     (* law: fix f tokens == f (fix f) tokens *)

  (* useful for building semantic functions *)
  val id   : 'a -> 'a
  val fst  : 'a * 'b -> 'a
  val snd  : 'a * 'b -> 'b
  val pair : 'a -> 'b -> 'a * 'b
  val eq   : ''a -> ''a -> bool

  val curry  : ('a * 'b      -> 'c) -> ('a -> 'b -> 'c)
  val curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)


  (* useful for wrapping producers, e.g., for debugging *)
  val transformWith :
        ('a producer_fun -> 'a producer_fun) -> ('a producer -> 'a producer)

end


(******* Using the fixed-point combinators *****

Suppose you want to define a recursive parser for an evaluator of
sums, like this:

    exp = int <|> succeed plus <~> the "(" <*> exp <~> the "+" <*> exp <~> the ")"

where `fun plus x y = x + y`.

You can't write this in ML.  But you can use a fixed-point combinator
that is just like the Y combinator in the lambda calculus.  First,
turn the recursion equation into a function that, when given `exp`,
returns `exp`:

    exp =  int <|> succeed plus <~> the "(" <*> exp <~> the "+" <*> exp <~> the ")"

 fn exp => int <|> succeed plus <~> the "(" <*> exp <~> the "+" <*> exp <~> the ")"

Then pass the whole thing to `fix`:
  
 fix (fn exp => int <|> succeed plus <~> the "(" <*> exp <~> the "+" <*> exp <~> the ")")

The result is your parser.

(The knot is tied using a mutable reference cell that is deferenced every
time tokens are delivered, so it's reasonably efficient.)

*)
