(* Playing with oborg. *)

open Core

let kf () =
  let password = Cstruct.of_string "kao3ohBae0quaMohzu5eemaeghei3Gox8zu" in
  let key = Keyfile.load_key ~password "/home/davidb/.config/borg/keys/lint_borgs_test" in
  Keyfile.Key.show key

(*
let () =
  let conf = Configfile.load "/lint/borgs/test/config" in
  printf "%s\n" (Configfile.show conf)
*)

type il = [`None | `Built of int | `Rebuild of (int * int array)] [@@deriving show]

let get_index repo =
  match Repo.last_segment repo with
    | `None -> failwith "TODO: Missing index"
    | `Rebuild _ -> failwith "TODO: Out of date index"
    | `Built n -> n

let () =
  match Sys.get_argv () with
    | [| _; path |] ->
      let repo = Repo.openrepo path in
      (* printf "%s\n" (Repo.show repo); *)
      let lseg = get_index repo in
      let hindex = Hashindex.Index.of_filename (Repo.index_file repo lseg) in
      Hashindex.Index.dump_info hindex;

      (* Verify that the hashes can be found. *)
      let count = ref 0 in
      Hashindex.Index.iter hindex ~f:(fun _key ->
        count := !count + 1);
      printf "Visited: %d\n" !count
    | _ -> failwith "Expecting a path argument"
