(* Hash index. *)

open Core

(* The implementation currently assumes that the hash is a Cstruct.t
 * of some fixed length, and that the value is at least 4 bytes, and
 * the two key values of LE 0xffffffff and 0xffffffe are not
 * meaningful (they are used to indicate empty and deleted buckets).
 *)

(* The index file has a header:
 * ofs len desc
 * --- --- ----
 * 0   8   magic value "BORG_IDX"
 * 8   4   used buckets
 * 12  4   total buckets
 * 16  1   key_len
 * 17  1   data_len
 * 18      first bucket
 *)

module type KV = sig
end

module Index = struct
  let key_len = 32
  let data_len = 8

  let first_bucket = 18
  let bucket_size = key_len + data_len

  type t = {
    data : Cstruct.t;
    used : int;
    buckets : int;
  }

  let dump_info t =
    printf "Hash index %d bytes, %d/%d buckets used (%%%.1f)\n"
      (Cstruct.len t.data)
      t.used t.buckets
      Float.(of_int t.used / of_int t.buckets * 100.0)

  let of_filename path =
    let fd = Unix.openfile path ~mode:[Unix.O_RDONLY] in
    let data = Unix_cstruct.of_fd fd in
    Unix.close fd;
    let magic = Cstruct.copy data 0 8 in
    if String.(magic <> "BORG_IDX") then
      failwith "Invalid magic on index file";
    let used = Int32.to_int_exn (Cstruct.LE.get_uint32 data 8) in
    let buckets = Int32.to_int_exn (Cstruct.LE.get_uint32 data 12) in
    let this_keylen = Cstruct.get_uint8 data 16 in
    if this_keylen <> key_len then
      failwith "Index file not of right type (key len mismatch)";
    let this_datalen = Cstruct.get_uint8 data 17 in
    if this_datalen <> data_len then
      failwith "Index file not of right type (data len mismatch)";
    let expected_len = first_bucket + bucket_size * buckets in
    if expected_len <> Cstruct.len data then
      failwith "Index file seems of incorrect length";
    { data; used; buckets }

  (* For testing purposes, iterate over all of the hashes, that are
   * not marked empty or deleted. *)
  let iter t ~f =
    for pos = 0 to t.buckets - 1 do
      let base = first_bucket + pos * bucket_size in
      let key = Cstruct.sub t.data base key_len in
      match Cstruct.LE.get_uint32 t.data (base + key_len) with
        | 0xfffffffel | 0xffffffffl -> ()
        | _ -> f key
    done
end
