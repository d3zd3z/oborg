(**
 * Repo management.
 *)

type t [@@deriving show]

val openrepo : string -> t

(** Given a segment number, return the pathname for the file
 * for that segment. *)
val segfile : t -> int -> string

(** Return the pathname for a given index file. *)
val index_file : t -> int -> string

(** Determine the segment number of the last segment.  Result
 * indicates if there are additional segment files beyond the latest
 * index.  `None indicates there is no index file. `Built n indicates
 * that there is an index file for segment n, and that is the last
 * segment found.  `Rebuild (n, rest) indicates that there is an
 * index, but there are also segment files 'rest' that need to be
 * rebuilt. *)
val last_segment : t -> [ `None | `Built of int | `Rebuild of int * int array ]

val get_key : t -> Keyfile.Key.t

(**
 * For testing, set an alternate home directory to look for keys, and
 * cache files.  This corresponds with the BORG_BASE environment
 * variable. *)
val set_base : string -> unit
val set_password : string -> unit
