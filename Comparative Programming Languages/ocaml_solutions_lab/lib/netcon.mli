(* Notice the absence of module type ... = sig ... end *)
(* OCaml will automatically add this for us when compiling this unit! *)

(* I redefine the con type to expose it outside the interface.
   Right now, it's not necessary, but you can imagine a function
   that allows us to set the connection type. Then we'd need to be
   able to pass values of type con as arguments to said function. *)
type con = Wired | Wireless | Bluetooth

type netcon

(** creates a new network connection *)
val create : unit -> netcon

(** establishes a connection from an inactive connection, fails otherwise *)
val establish : netcon -> string -> netcon

(** disconnects from an active connection, fails otherwise *)
val disconnect : netcon -> netcon

(** Sets bandwidth on active connections, fails otherwise *)
val bandwidth : netcon -> int -> netcon

(** produces a string representation of the supplied network connection *)
val string_of_netcon : netcon -> string

(* The following is needed to implement the module argument for Set.Make *)
(** compares two network connections. Inactives are compared by last connection date,
    Actives are compared by bandwidth *)
val compare : netcon -> netcon -> int