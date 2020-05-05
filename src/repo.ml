(* Repo management. *)

open Core

type t = {
  path : string;
  conf : Configfile.t;
}
[@@deriving show]

module Env = struct
  type t = {
    base : string option;
    password : Cstruct.t option;
  }

  let empty_env () = { base = None; password = None }

  let default_env () =
    let base = Some (Sys.home_directory ()) in
    let password = Sys.getenv "BORG_PASSPHRASE" |> Option.map ~f:Cstruct.of_string in
    { base; password }

  type key = [ `Base | `Password ]

  let set_env env key data =
    match key with
      | `Base -> { env with base = Some data }
      | `Password -> { env with password = Some (Cstruct.of_string data) }
end

let get_key t =
  Option.value_exn t.conf.key

(* This handles the mangling, as long as the path is canonical, and is
 * a local filename.  This replaces '/' and '.' in the name with
 * underscores.  It might replace other characters as well. *)
let mangle_name path =
  let path = String.substr_replace_all ~pattern:"." ~with_:"_" path in
  let ff = match String.split ~on:'/' path with
    | (""::xs) -> xs
    | xs -> xs in
  String.concat ~sep:"_" ff

let openrepo (env : Env.t) path =
  let password = Option.value env.password ~default:(Cstruct.empty) in
  let home_dir = Option.value_exn env.base in

  let conf = Configfile.load ~password (path ^/ "config") in
  let conf = if Option.is_some conf.key then conf
  else begin
    let name = mangle_name path in
    (* TODO: Use Home directory, not this path. *)
    let name = home_dir ^/ ".config/borg/keys" ^/ name in
    let key = Keyfile.load_key ~password name in
    { conf with key = Some key }
  end in
  { path; conf }

let segfile t n =
  sprintf "%s/data/%d/%d" t.path
    (n / t.conf.segments_per_dir)
    n

let index_file t n =
  sprintf "%s/index.%d" t.path n

let index_re = Re.compile (Re.Pcre.re {|^index\.(\d+)$|})
let number_re = Re.compile (Re.Pcre.re {|^(\d+)$|})

let get_index_files t =
  let names = Sys.readdir t.path in
  let names = Array.filter_map names ~f:(Re.exec_opt index_re) in
  Array.map names ~f:(fun p -> Re.Group.get p 1 |> Int.of_string)

(* Retrieve an array of all of the numbered files or dirs in a given
 * directory. *)
let get_numbered path =
  Sys.readdir path
    |> Array.filter_map ~f:(Re.exec_opt number_re)
    |> Array.map ~f:(fun p -> Re.Group.get p 1 |> Int.of_string)

let get_segment_files t =
  let dirs = get_numbered (t.path ^/ "data") in
  let names = Array.map dirs ~f:(fun num ->
    get_numbered (t.path ^/ "data" ^/ Int.to_string num)) in
  let names = Array.concat (Array.to_list names) in
  Array.sort names ~compare:Int.compare;
  names

(* Currently, this doesn't check that the segments mentioned in the
 * index exist.  We mainly care if there are segments beyond the
 * index, which means the index will need to be rebuilt. *)
let last_segment t =
  (* Determine highest numbered index file. *)
  match Array.max_elt ~compare:Int.compare (get_index_files t) with
    | None -> `None
    | Some n ->
        let segs = get_segment_files t in
        let segs = Array.filter ~f:(fun x -> x > n) segs in
        if Array.is_empty segs then
          `Built n
        else
          `Rebuild (n, segs)
