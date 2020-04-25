(* Repo management. *)

open Core

type t = {
  path : string;
  conf : Configfile.t;
}
[@@deriving show]

(* For now, we only handle local repos, with either a repokey or a
 * separate keyfile. *)

let getpass () =
  Sys.getenv "BORG_PASSPHRASE"
  |> Option.value_map ~default:Cstruct.empty ~f:Cstruct.of_string

(* This handles the mangling, as long as the path is canonical, and is
 * a local filename. *)
let mangle_name path =
  let ff = match String.split ~on:'/' path with
    | (""::xs) -> xs
    | xs -> xs in
  String.concat ~sep:"_" ff

let openrepo path =
  let password = getpass () in

  let conf = Configfile.load ~password (path ^/ "config") in
  let conf = if Option.is_some conf.key then conf
  else begin
    let name = mangle_name path in
    (* TODO: Use Home directory, not this path. *)
    let name = "/home/davidb/.config/borg/keys" ^/ name in
    let key = Keyfile.load_key ~password name in
    { conf with key = Some key }
  end in
  { path; conf }

let segfile t n =
  sprintf "%s/data/%d/%d" t.path
    (n mod t.conf.segments_per_dir)
    n
