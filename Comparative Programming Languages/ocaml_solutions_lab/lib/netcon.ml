(* Notice the absence of module ... = struct ... end *)
(* OCaml will automatically add this for us when compiling this unit! *)

type con = Wired | Wireless | Bluetooth

type netcon = 
  | Active of {
      con_type    : con;
      dest_ip     : string;
      bandwidth   : int;
      established : string;
    }
  | Inactive of {
      last_active : string option
    }

let create () = Inactive { last_active = None }

let establish nc ip = match nc with
| Active _ -> failwith "Cannot establish a connection that is already active!"
| Inactive _ -> Active
    { con_type = Wired (* Arbitrary choice *)
    ; dest_ip = ip
    ; bandwidth = 0
    ; established = "Now" (* stubbed for a system call to fetch current date/time *)
    } 

let disconnect = function
  | Active _ -> Inactive { last_active = Some "A second ago!" } (* idem *)
  | Inactive _ -> failwith "Cannot disconnect an inactive connection!"

let bandwidth nc bw = match nc with
  | Active r -> Active { r with bandwidth = bw } (* Keeps all fields the same except bandwidth *)
  | Inactive _ -> failwith "Cannot set bandwidth of inactive connection!"

let string_of_netcon = function
  | Active r -> "Active connection to " ^ r.dest_ip
  | Inactive _ -> "Inactive connection"

let compare nc1 nc2 = match nc1, nc2 with
  | Inactive a, Inactive b -> compare a.last_active b.last_active
  | Active a, Active b -> compare a.bandwidth b.bandwidth
  | _, _ -> 0