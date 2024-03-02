open Exercises (* Exercises is the name of the project `exercises` with a capital E *)

let () = let open Netcon in
  create () |> string_of_netcon |> print_string

module OrderedNetcon = struct
  type t = Netcon.netcon
  (* In the above, notice the doubling of the word "netcon".
     Idiomatic OCaml would name the "carrier type" netcon
     inside the netcon module simply `t`. This would simplify here to
     type t = Netcon.t 
     
     Over time, you learn to skip everything starting from the `.`:
     it becomes kind of like the parentheses in Lisp dialects,
     you just don't read the `.t` part any more.

     `type t = Netcon.t` then reads as ...
     "the abstract type t is just the type declared in the Netcon module"
     *) 
  let compare nc1 nc2 = Netcon.compare nc1 nc2
end

(* Notice the type signature of NetconSet, it has the usual operations that we
   expect a Set/Dictionary to have, but specialized for Netcons! *)
module NetconSet = Set.Make (OrderedNetcon)

(* Some might say that functional programming is all about defining usable combinators.
   Entire libraries are written with this idea. They expose a "DSL" (domain-specific language)
   Which is really just a set of combinators that make it "intuitive" to write programs in a strange made-up language.

   Functional programming has its roots in combinatory logic (after all, Haskell Curry is considered a logician!)
   If you like birds and logic puzzles, I can recommend the book "To Mock a Mockingbird (1985)" by R. Smullyan.
   There, (>>) is expressed as the Queer bird (Q) (page 105). In SKI: ((S(K(S((S(KS))K))))K).
   Rolls right off the tongue, doesn't it... *)

let (>>) f g x = g(f(x))

let () = NetconSet.empty |> NetconSet.add (Netcon.create ())
      |> NetconSet.iter (Netcon.string_of_netcon >> print_string)

