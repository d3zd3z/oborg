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

let index_file t n =
  sprintf "%s/index.%d" t.path n

let index_re = Re.compile (Re.Pcre.re {|^index\.(\d+)$|})

let get_index_files t =
  let names = Sys.readdir t.path in
  let names = Array.filter_map names ~f:(Re.exec_opt index_re) in
  Array.map names ~f:(fun p -> Re.Group.get p 1 |> Int.of_string)

(* Currently, this doesn't check that the segments mentioned in the
 * index exist.  We mainly care if there are segments beyond the
 * index, which means the index will need to be rebuilt. *)
let last_segment t =
  (* Determine highest numbered index file. *)
  match Array.max_elt ~compare:Int.compare (get_index_files t) with
    | None -> `None
    | Some n ->
        let last_seg =
          let rec loop i =
            if Sys.file_exists_exn (index_file t i) then
              loop (i + 1)
            else (i - 1) in
          loop (n + 1) in
        if last_seg > n then `Rebuild (n, last_seg)
        else `Built n

