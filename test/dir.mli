(**
 * Creation and management of test directories. *)

(** The directory type. *)
type t = string

(** Construct a test directory.  Will also create "repo" and "base"
 * directories within it. *)
val make : unit -> t

(** Construct a test directory, run a sequence, and then clean it up
 * when we are done.  If f raises an exception, that exception will be
 * reraised after the directory is cleaned up. *)
val with_dir : f:(t -> 'a) -> 'a

(**
 * Run an internal test of this module. *)
val runtest : OUnit2.test_ctxt -> unit
