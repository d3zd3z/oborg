(**
 * Repo management.
 *)

type t [@@deriving show]

module Env : sig
  (**
   * Open a repo depends on some values that Borg passes in through the
   * environment (along with defaults).  To support testing, we allow
   * these to come from either the environment, the environment,
   * modified by the command line, or just set (as in a test). *)
  type t

  (** Query the environment to get values. *)
  val default_env : unit -> t

  (** Start with an empty environment (used by testing to make sure
   * these values do not come from the real environment. *)
  val empty_env : unit -> t

  (** Possible values set in the environment. *)
  type key = [ `Base | `Password ]

  (** Modify a value.  This can be either an override. *)
  val set_env : t -> key -> string -> t
end

val openrepo : Env.t -> string -> t

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
