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

(* Hash size table.  This is copied out of the borg code (with the
 * values made into Int32s).  The values were chosen to fit in a
 * signed 32-bit int. *)
let hash_sizes = [|
    1031; 2053; 4099; 8209; 16411; 32771; 65537; 131101; 262147; 445649;
    757607; 1287917; 2189459; 3065243; 4291319; 6007867; 8410991;
    11775359; 16485527; 23079703; 27695653; 33234787; 39881729; 47858071;
    57429683; 68915617; 82698751; 99238507; 119086189; 144378011; 157223263;
    173476439; 190253911; 209915011; 230493629; 253169431; 278728861;
    306647623; 337318939; 370742809; 408229973; 449387209; 493428073;
    543105119; 596976533; 657794869; 722676499; 795815791; 874066969;
    962279771;
    (*
     * Realistically, we can't even get close to these sizes on a
     * 32-bit platform.  Since the offsets to the Cstruct are 'int'
     * values, we won't be able to index past these values on these
     * platforms.  We'll leave these in for now, so that we get a
     * clear overflow of a constant at compile time on a 32-bit
     * platform.
     *)
    1057701643; 1164002657; 1280003147; 1407800297; 1548442699;
    1703765389; 1873768367; 2062383853 |]

module type KV = sig
  type data
  val key_len : int
  val data_len : int

  val get_data : Cstruct.t -> int -> data
end

module type INDEX = sig
  type data
  type t
  val of_filename : string -> t
  val write_file : t -> string -> unit
  val make_empty : unit -> t
  val find : t -> key:Cstruct.t -> data option

  val dump_info : t -> unit
  val iter : t -> f:(Cstruct.t -> unit) -> unit
  val lookup_test : t -> unit
end

module Make_index (Data : KV) : INDEX with type data = Data.data = struct

  type data = Data.data

  let first_bucket = 18
  let bucket_size = Data.key_len + Data.data_len

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

  let pos_of_key t ~key =
    (* This is a bit messy as we don't have real unsigned ints in
     * ocaml.  We can take advantage of the int63 type, which will
     * either be a regular int on a 64-bit platform, or a 64 bit value
     * on 32-bit platforms. *)
    let pos = Cstruct.LE.get_uint32 key 0 in
    let pos = Int63.(of_int32 pos land of_int64_exn 0xffffffffL) in
    Int63.to_int_exn Int63.(rem pos (of_int t.buckets))

  let get_key t pos =
    let base = first_bucket + pos * bucket_size in
    Cstruct.sub t.data base Data.key_len

  let key_equal t pos key =
    Cstruct.equal key (get_key t pos)

  let get_flag t pos =
    let base = first_bucket + pos * bucket_size in
    Cstruct.LE.get_uint32 t.data (base + Data.key_len)

  let set_flag t pos value =
    let base = first_bucket + pos * bucket_size in
    Cstruct.LE.set_uint32 t.data (base + Data.key_len) value

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
    if this_keylen <> Data.key_len then
      failwith "Index file not of right type (key len mismatch)";
    let this_datalen = Cstruct.get_uint8 data 17 in
    if this_datalen <> Data.data_len then
      failwith "Index file not of right type (data len mismatch)";
    let expected_len = first_bucket + bucket_size * buckets in
    if expected_len <> Cstruct.len data then
      failwith "Index file seems of incorrect length";
    { data; used; buckets }

  (* The field "used buckets" is set as part of write.  All other
   * fields should have already been initialized. *)
  let write_file t path =
    Cstruct.LE.set_uint32 t.data 8 (Int32.of_int_exn t.used);
    let tname = path ^ ".tmp" in
    let fd = Unix.openfile tname ~mode:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] in
    let count = Bigstring.write fd (Cstruct.to_bigarray t.data) in
    if count <> Cstruct.len t.data then
      failwith "Unable to write file";
    Unix.rename ~src:tname ~dst:path

  let make_sized buckets =
    let bsize = first_bucket + bucket_size * buckets in
    let data = Cstruct.create bsize in
    Cstruct.blit_from_string "BORG_IDX" 0 data 0 8;
    Cstruct.LE.set_uint32 data 12 (Int32.of_int_exn buckets);
    Cstruct.set_uint8 data 16 Data.key_len;
    Cstruct.set_uint8 data 17 Data.data_len;
    let t = { data; used = 0; buckets } in
    for i = 0 to buckets - 1 do
      set_flag t i 0xffffffffl
    done;
    t

  let make_empty () =
    make_sized hash_sizes.(0)

  let iter t ~f =
    for pos = 0 to t.buckets - 1 do
      let base = first_bucket + pos * bucket_size in
      let key = get_key t pos in
      match Cstruct.LE.get_uint32 t.data (base + Data.key_len) with
        | 0xfffffffel | 0xffffffffl -> ()
        | _ -> f key
    done

  let find t ~key =
    let pos = pos_of_key t ~key in
    let rec loop pos =
      let base = first_bucket + bucket_size * pos in
      if key_equal t pos key then begin
        Some (Data.get_data t.data (base + Data.key_len))
      end else begin
        match get_flag t pos with
          | -1l ->
              (* Indicates an empty bucket, which is our only stop
               * condition. *)
              None
          | _ ->
              (* Otherwise, it is either the wrong key, or the record
               * was deleted. *)
              let pos = pos + 1 in
              let pos = if pos = t.buckets then 0 else pos in
              loop pos
      end in
    loop pos

  let twiddle key offset =
    let key = Cstruct.copy key 0 Data.key_len in
    let key = Cstruct.of_string key in
    let data = Cstruct.get_uint8 key offset in
    Cstruct.set_uint8 key offset (data lxor 1);
    key

  let lookup_test t =
    let count = ref 0 in
    iter t ~f:(fun key ->
      begin match find t ~key with
        | Some _ -> count := !count + 1
        | None -> failwith "Unable to find key" end;
      let key2 = twiddle key (Data.key_len - 1) in
      begin match find t ~key:key2 with
        | Some _ -> failwith "Should not have found twiddled key"
        | None -> () end;
      let key3 = twiddle key 0 in
      begin match find t ~key:key3 with
        | Some _ -> failwith "Should not have found 0 twiddled key"
        | None -> () end);
    assert (!count = t.used)
end
