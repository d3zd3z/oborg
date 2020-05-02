(* Creation and management of test directories.
 *
 * For a typical test, we need a place to create a Borg directory.
 * Since borg places several directories in the user's home directory,
 * we want to override these as well.
 *
 * Fortunately, this can be overridden by setting BORG_BASE_DIR to an
 * alternative directory.
 *
 * We will create a new directory in a temp location, and within that,
 * have two directories:
 * - `repo`: The Borg repo itself.
 * - `base`: The location set for the BORG_BASE_DIR.  By having this
 * set in the environment when we invoke borg, it will use these
 * directories.  We will use an internal mechanism in our library to
 * override the default directories, since Ocaml seems to just read
 * the environment, and not be able to write to it.
 *)

open Core

(* A temp directory is a location with the above directories. *)
type t = string

let base_name = "base"
let repo_name = "repo"

let make () =
  let tdir = Filename.temp_dir ~in_dir:"/var/tmp" "oborg" "" in
  Unix.mkdir (tdir ^/ base_name);
  Unix.mkdir (tdir ^/ repo_name);
  tdir

let with_dir ~f =
  let t = make () in
  let ex = try Ok (f t) with
    | ex -> Error ex in
  if Option.is_none (Sys.getenv "OBORG_KEEP") then
    FileUtil.rm ~recurse:true [t];
  match ex with
    | Ok result -> result
    | Error ex -> raise ex

let runtest () =
  let remember = ref None in
  with_dir ~f:(fun dir ->
    remember := Some dir;
    assert (Sys.is_directory_exn (dir ^/ base_name));
    assert (Sys.is_directory_exn (dir ^/ repo_name));
    ());
  assert (not (Sys.is_directory_exn (Option.value_exn !remember)))
