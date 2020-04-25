(**
 * Repo management.
 *)

type t [@@deriving show]

val openrepo : string -> t

(** Given a segment number, return the pathname for the file
 * for that segment. *)
val segfile : t -> int -> string
