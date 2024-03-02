(* WARMUP *)

(* Exercise 1: 
Write a function last : 'a list -> 'a option that returns the last element of a list *)

let rec last : 'a list -> 'a option =
  fun l -> match l with
  | []  -> None
  | [x] -> Some x
  | _ :: t -> last t

(* Alternative solution using `function` keyword: *)
let rec last : 'a list -> 'a option = function
  | []  -> None
  | [x] -> Some x
  | _ :: t -> last t

(* Due to HM type inference, we can omit type annotations: *)
let rec last = function
  | []  -> None
  | [x] -> Some x
  | _ :: t -> last t

(* Faulty implementation, but still compiles...
   notice the type signature that was inferred by the OCaml compiler:
   'a option list -> 'a option *)
let rec faulty_last = function
  | []  -> None
  | [x] -> x (* Notice the missing Some constructor *)
  | _ :: t -> faulty_last t

(* This works because the type of [] is 'a list!
   Meaning the empty list can be inserted as a value for /any/ list
   no matter the type of values inside
   
   In particular, it can be used for a list of optional values of type 'a
   In OCaml speak an... 'a option list.
   
   This is consistent with the pattern [x] -> x, which can return an optional value...
   provided that x is itself an option! *)

(* ========================== *)

(* Exercise 2: 
Find out whether a list is a palindrome. Hint: A palindrome is its own reverse *)

(* read these 3 functions in reverse order *)

(* appending two lists is just adding the first element of the left list to the right list 
  until there are no more elements to append. *)
let rec list_append l1 l2 = match l1 with
| [] -> l2
| h :: t -> list_append t (h :: l2)

(* the reverse of a list is exactly produced by appending it element-wise to an empty list *)
let reverse l = list_append l []

(* Using the tip: a palindrome is its own reverse *)
let is_palindrome l = reverse l = l

(* P.s. palindromes are really useful! Also in Computer Science.
   Johan Jeuring has dedicated an entire blog (and research career!) to the problem 
   http://finding-palindromes.blogspot.com/2012/10/the-history-of-finding-palindromes.html *)

(* ========================== *)

(* Exercise 3: 
Define a polymorphic map function that given an argument f : 'a -> 'b
and a list l : 'a list applies f to every element of l. *)

(* Mapping and folding over lists is deeply functional! 
   There are entire paradigms and systems written in this way. See e.g. 
   https://en.wikipedia.org/wiki/MapReduce 
   (reduce is just a lame way to say "fold") 
*)

(* Here is an idiomatic OCaml solution (except for the explicit type annotations) *)
let rec map : ('a -> 'b) -> 'a list -> 'b list = fun f -> function
  | [] -> []
  | h :: t -> f h :: map f t

(* Did you know that map can be defined in terms of fold/reduce? 
   I guess Google could have just named their technology "Reduce", since
   map can be expressed as a fold... *)

let map f l = List.fold_right (fun xs x -> xs @ [f x]) [] l

(* In the solution above, I'm using OCaml's built-in List module; it has a
   function fold_left that folds/reduces over lists.
   A map is precisely a fold that produces a new list where every element is appended (@)
   at the back after having been applied to `f`. *)

(* If you're interested, Graham Hutton has a great paper on this!
   https://www.cs.nott.ac.uk/~pszgmh/fold.pdf *)


(* ========================== *)

(* Exercise 4: Define a data type that enumerates different connection types: wired,
wireless, via bluetooth, ... *)

type con = Wired | Wireless | Bluetooth

(* In OCaml, data types start with a lowercase letter and constructors (so values) of the 
   data type use capital letters *)

(* ========================== *)

(* Exercise 5: Define a record type that captures a network connection *)

type netcon = {
  active    : bool;
  con_type  : con;
  dest_ip   : string;
  bandwidth : int;
  date      : string;
}

(* This "solution" is kind of the C way of defining things: you put everything
   inside of a `struct` and then write a big /* DO NOT SET BANDWIDTH >0 IF active is false */
   comment and pray that no-one violates these invariants *)

(* In OCaml, we can do something much cooler... *)

(* ========================== *)

(* Exercise 6: Make use of variant types to enforce the invariants in the previous exercise.
It should no longer be possible to set e.g. bandwidth to non-zero if there is
no active connection *)

type netcon = 
  | Active of {
      con_type    : con;
      dest_ip     : string;
      bandwidth   : int;
      established : string;
    }
  | Inactive of {
      last_active : string
    }

(* What I've done is used variant types to make it /impossible/ (by construction) 
   to create invalid objects. We simply cannot pass e.g. a bandwidth value to an
   Inactive connection!  *)

(* During one of the exercise sessions, a student raised a great question.
   Q: We're not really enforcing a bandwidth of 'zero' in the Inactive case! 
      All we're saying is that there's no bandwidth to set. It's still implicit
      that the bandwidth is zero for inactive connections!

   A: That's very true. We would like to be able to say something like:
      `Inactive has a bandwidth field, but its value must always equal 0!`
      Unfortunately, we cannot enforce that types can rely on 
      values in OCaml's type system. We would need a more expressive type system
      that can in a sense "refine" which values of a type are allowed.
      Such a type is known as a refinement type. It would allow us to express e.g.
      `bandwidth : [ x ∈ int | x = 0 ]` to mean that bandwidth can take values `x` of
      type `int`, provided that `x` is equal to zero.

      Extending OCaml's type system in this way is possible, but in practice it would
      lead to undecidable type inference.
      That means we would lose a very nice feature of the language (not having to tell
      the compiler about all the types all the time), in order to make specifications
      more expressive.
      If you take this idea a bit further, you end up with dependent types such as those
      found in Agda, Coq, Idris, Lean, ... (If you are taking prof. Dominique Devriese's
      course on Formal Systems and their applications this might ring a bell). 
      In these systems, the types are so expressive that we could encode literally
      every conceived mathematical statement and every conceivable mathematical statement
      as a type in the system.
      Programs that have these types are then precisely proofs of those statements!
      https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence
      *)


(* ========================== *)

(* Exercise 7:
Define a data type for rose trees. A rose tree is a type of tree that has
arbitrary many subtrees. Moreover, your data type should be polymorphic
in the type of values that the tree contains. *)

type 'a rose_tree = Thorn of 'a * 'a rose_tree list
      
(* ========================== *)

(* Exercise 8:
Define a function most_beautiful_rose that given an arbitrary rose tree,
finds the subtree with the highest branching factor. *)

let most_beautiful_rose (t : 'a rose_tree) =
  let open List in 
  let rec branching (Thorn (_, ts) as t) =
    ts |> fold_left (fun ((acc, _) as curr) t ->
            match branching t with (max, _) as cand when (max > acc) -> cand | _ -> curr)
         (length ts, t)
  in branching t |> snd 

(* We open the List module locally (so e.g. List.length is now unqualified as simply "length")
   Then we define an inner/helper function `branching` with an explicit accumulator argument t,
   which is immediately destructed to reveal its subtrees `ts`
   Deciding the highest branching factor is simply a left fold over these subtrees
   Where the current maximum is kept in a tuple `curr` of the max length and that subtree
   whenever we come across a new candidate `cand`, we check whether its maximum branch is strictly larger.
   if so, we continue folding with this candidate. If not, we keep the existing max `curr`
   at the end, we are not interested in the max length, only the subtree that is the second component of the pair *)

(* This solution uses a bunch of OCaml syntax that's easy to get lost in. Take your time to explore what it all does!
   Recall e.g. that one can immediately destruct a value when binding it to a name:
   let (x, y) = fun_that_returns_a_tuple () in ...
   
   Equally, we can give the tuple itself a name (such as `t`):
   let (x, y) as t = fun_that_returns_a_tuple () in ...
   
   This corresponds roughly to...
   let t = fun_that_returns_a_tuple () in
   let (x,y) = t in ...
    
   or equally...
   let t = fun_that_returns_a_tuple () in
   match t with (x,y) -> ...

   or equally ...
   match fun_that_returns_a_tuple () with (x,y) as t -> ...

   *)