(**
 * Configuration file.
 *
 * Read the repo config file into a local data structure.  For repos
 * with 'repokey' the key is stored (password protected) in the
 * configfile itself.  Otherwise the 'keyfile' mode stores the key in
 * a separate file outside of the repo.
 *)

type t = {
  version : int;
  segments_per_dir : int;
  max_segment_size : int;
  append_only : bool;
  id : Cstruct.t;

  storage_quota : int option;
  additional_free_space : int option;
  key : Keyfile.Key.t option;
}
[@@deriving show]

val load : string -> t
