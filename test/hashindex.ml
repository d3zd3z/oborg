(* Hashindex testing. *)

open Core

module SHA256 = Mirage_crypto.Hash.SHA256
module ISet = Set.Make (Int)

module Ipair = struct
  (* The Hashindex.KV contract *)
  type data = (int * int)
  let key_len = 32
  let data_len = 8
  let get_data bin offset =
    let first = Int32.to_int_exn (Cstruct.LE.get_uint32 bin offset) in
    let second = Int32.to_int_exn (Cstruct.LE.get_uint32 bin (offset + 4)) in
    (first, second)
  let set_data bin offset (first, second) =
    Cstruct.LE.set_uint32 bin offset (Int32.of_int_trunc first);
    Cstruct.LE.set_uint32 bin (offset + 4) (Int32.of_int_trunc second)

  (* Generating a hash for testing. *)
  let get_hash (first, second) =
    let buf = Cstruct.create_unsafe 8 in
    Cstruct.LE.set_uint32 buf 0 (Int32.of_int_trunc first);
    Cstruct.LE.set_uint32 buf 4 (Int32.of_int_trunc second);
    SHA256.digest buf
end

module Index = Borg.Hashindex.Make_index (Ipair)

(* Add entries [n,m) to the table, augmenting the local set to contain
 * the entries we have inserted. *)
let add_entries hi s n m =
  let rec loop s i =
    if i >= m then s
    else begin
      let s = ISet.add s i in
      let key = Ipair.get_hash (i, i) in
      Index.insert hi ~key ~data:(i, i);
      loop s (i + 1)
    end in
  loop s n

(* Check entities to make sure that they are properly present in the
 * set. *)
let check_entries hi s =
  let s = ref s in
  Index.iter hi ~f:(fun key ->
    match Index.find hi ~key with
      | None -> failwith "Unable to find key"
      | Some (a, b) ->
          if a <> b then failwith "Invalid data item";
          let key2 = Ipair.get_hash (a, b) in
          if not (Cstruct.equal key key2) then
            failwith "Key and data mismatch";
          if not (ISet.mem !s a) then
            failwith "Found entry that shouldn't be present";
          s := ISet.remove !s a);
  if not (ISet.is_empty !s) then
    failwith "Items missing from hashindex"

let runtest _ =
  let to_add = 500 in
  Dir.with_dir ~f:(fun dir ->
    let hiname = dir ^/ "index.0" in
    let h = Index.make_empty () in
    let s = add_entries h ISet.empty 1 (to_add + 1) in
    check_entries h s;
    Index.write_file h hiname;
    let h2 = Index.of_filename hiname in
    check_entries h2 s;
    ())
