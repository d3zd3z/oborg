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

module SegmentKV : Hashindex.KV with type data = (int * int) = struct
  type data = (int * int)
  let key_len = 32
  let data_len = 8
  let get_data bin offset =
    let first = Int32.to_int_exn (Cstruct.LE.get_uint32 bin offset) in
    let second = Int32.to_int_exn (Cstruct.LE.get_uint32 bin (offset + 4)) in
    (first, second)
end

module Index = Hashindex.Make_index (SegmentKV)

let () =
  match Sys.get_argv () with
    | [| _; path |] ->
      let repo = Repo.openrepo path in
      (* printf "%s\n" (Repo.show repo); *)
      let lseg = get_index repo in
      let hindex = Index.of_filename (Repo.index_file repo lseg) in
      Index.dump_info hindex;

      Index.lookup_test hindex
    | _ -> failwith "Expecting a path argument"
