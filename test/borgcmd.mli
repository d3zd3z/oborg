(* Invoking borg *)

val modes : string list

val init : Dir.t -> string -> unit
val create : Dir.t -> string -> string -> unit

val runtest : OUnit2.test_ctxt -> unit
