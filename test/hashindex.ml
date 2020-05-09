(* Hashindex testing. *)

open Core

module SHA256 = Mirage_crypto.Hash.SHA256

module Ipair = struct
  (* The Hashindex.KV contract *)
  type data = (int * int)
  let key_len = 32
  let data_len = 8
  let get_data bin offset =
    let first = Int32.to_int_exn (Cstruct.LE.get_uint32 bin offset) in
    let second = Int32.to_int_exn (Cstruct.LE.get_uint32 bin (offset + 4)) in
    (first, second)

  (* Generating a hash for testing. *)
  let get_hash (first, second) =
    let buf = Cstruct.create_unsafe 8 in
    Cstruct.LE.set_uint32 buf 0 (Int32.of_int_trunc first);
    Cstruct.LE.set_uint32 buf 4 (Int32.of_int_trunc second);
    SHA256.digest buf
end

module Index = Borg.Hashindex.Make_index (Ipair)

let runtest _ =
  Dir.with_dir ~f:(fun dir ->
    let hiname = dir ^/ "index.0" in
    let _ = Ipair.get_hash (0, 0) in
    let h = Index.make_empty () in
    Index.write_file h hiname;
    let h2 = Index.of_filename hiname in
    Index.iter h2 ~f:(fun _ -> failwith "Element in empty hashindex");
    ())
