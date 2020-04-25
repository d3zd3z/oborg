(* Keyfiles
 *
 * Borg stores the keys for backups in a password-protected file.  It
 * is a bespoke format defined by Borg.  This module is able to read
 * these keyfiles to make the keys available.
 *)

open Core

module SM = Map.Make (String)
module Hash = Mirage_crypto.Hash
module PB = Pbkdf.Make (Hash.SHA256)
module AESCTR = Mirage_crypto.Cipher_block.AES.CTR

(*
 * A utility unpacker.
 *)
module Unpacker = struct
  let of_int f = match f with
    | Msgpck.Int x -> x
    | _ -> failwith "failure"
  let of_int32 f = match f with
    | Msgpck.Int x -> (Int32.of_int_exn x)
      (* TODO: Maybe encodes differently? *)
    | _ -> failwith "failure"
  let of_cstruct f = match f with
    | Msgpck.String x -> Cstruct.of_string x
    | _ -> failwith "failure"
  let of_string f = match f with
    | Msgpck.String x | Bytes x -> x
    | _ -> failwith "failure"
  let of_bool f = match f with
    | Msgpck.Bool x -> x
    | _ -> failwith "failure"

  let get fields (msg : Msgpck.t) =
    let kvs = match msg with
      | Map kvs -> kvs
      | _ -> failwith "Expecting Map" in
    let kvs = List.map kvs ~f:(fun (k, v) -> match k with
      | String k -> (k, v)
      | _ -> failwith "Expecting string key")in
    let kvs = match SM.of_alist kvs with
      | `Ok kvs -> kvs
      | `Duplicate_key k -> failwith ("Duplicate key: " ^ k) in
    List.map fields ~f:(fun (name, _) ->
      match SM.find kvs name with
        | None -> failwith "Field missing in msgpack"
        | Some x -> x)

  (* Reexport Cstruct, with a 'pp' function. *)
  module Cstruct = struct
    include Cstruct
    let pp = hexdump_pp
  end
end

(**
 * The unwrapped keys.  Secret data is kept in a Cstruct.t to keep it
 * from being moved by the garbage collector. *)
module Key = struct
  open Unpacker

  type t = {
    version : int;
    chunk_seed : int32;
    enc_hmac_key : Cstruct.t;
    enc_key : Cstruct.t;
    id_key : Cstruct.t;
    repository_id : string;
    tam_required : bool;
  }
  [@@deriving show]

  let fields = [
    ("version", `Int);
    ("chunk_seed", `Int32);
    ("enc_hmac_key", `Cstruct);
    ("enc_key", `Cstruct);
    ("id_key", `Cstruct);
    ("repository_id", `String);
    ("tam_required", `Bool)]

  let of_msg msg =
    match get fields msg with
      | [version; chunk_seed; enc_hmac_key; enc_key; id_key;
        repository_id; tam_required] ->
          { version = of_int version;
            chunk_seed = of_int32 chunk_seed;
            enc_hmac_key = of_cstruct enc_hmac_key;
            enc_key = of_cstruct enc_key;
            id_key = of_cstruct id_key;
            repository_id = of_string repository_id;
            tam_required = of_bool tam_required }
      | _ -> failwith "field mismatch"
end

(**
 * The wrapped key holds the information needed to unwrap the key,
 * provided the proper password is given.
 *)
module KeyWrap = struct
  open Unpacker

  type alg = SHA256
  (* [@@deriving show] *)

  let alg_of_string = function
    | "sha256" -> SHA256
    | _ -> failwith "Unsupported algorithm"

  type t = {
    version : int;
    salt : Cstruct.t;
    iterations : int;
    algorithm : alg;
    hash : Cstruct.t;
    data : Cstruct.t;
  }
  (* [@@deriving show] *)

  let fields = [
    ("version", `Int);
    ("salt", `Cstruct);
    ("iterations", `Int);
    ("algorithm", `Alg);
    ("hash", `Cstruct);
    ("data", `Cstruct)]

  let of_msg msg =
    match get fields msg with
      | [version; salt; iterations; algorithm; hash; data] ->
          { version = of_int version;
            salt = of_cstruct salt;
            iterations = of_int iterations;
            algorithm = alg_of_string (of_string algorithm);
            hash = of_cstruct hash;
            data = of_cstruct data }
      | _ -> failwith "field mismatch"

  (* Given a passphrase, decode and validate the wrapped key. *)
  let decode ~password t =
    let buf = PB.pbkdf2 ~password ~salt:t.salt ~count:t.iterations ~dk_len:32l in
    let key = AESCTR.of_secret buf in
    let plain = AESCTR.decrypt ~key ~ctr:(0L,0L) t.data in

    (* Validate hmac *)
    let digest = Hash.SHA256.hmac ~key:buf plain in
    if not (Cstruct.equal t.hash digest) then
      failwith "Digest mismatch on key, invalid passphrase?";
    Cstruct.to_string plain
end

let splitit line =
  match String.split line ~on:' ' with
    | [key; repoid] -> key, repoid
    | _ -> failwith "Invalid first line"

let from_bin ~password bin =
  let pos, payload = Msgpck.String.read bin in
  if pos <> String.length bin then
    failwith "Trailing garbage in base64 key";

  let wrap = KeyWrap.of_msg payload in
  (* printf "wrap: %s\n" (KeyWrap.show wrap); *)
  let plain = KeyWrap.decode ~password wrap in
  let pos, plainmsg = Msgpck.String.read plain in
  if pos <> String.length plain then
    failwith "Trailing garbage in encoded key";

  let plain = Key.of_msg plainmsg in

  (* Verify that the incoming repo matches the wrapped one. *)
  (* TODO: Put this back. *)
  (*
  let repoid = Cstruct.to_string (Cstruct.of_hex repoid) in
  if not (String.equal repoid plain.repository_id) then
    failwith "Wrapped repo ID mismatch with outer ID";
  *)

  plain

let from_base64 ~password text =
  let bin = Base64.decode_exn text in
  from_bin ~password bin

let load_key ~password path =
  let lines = In_channel.read_lines path in
  let key, _repoid, lines = match lines with
    | (x::xs) ->
        let key, repoid = splitit x in
        (key, repoid, xs)
    | _ -> failwith "No lines present in key" in

  if String.(key <> "BORG_KEY") then
    failwith (sprintf "Unexpected tag %S instead of BORG_KEY" key);

  from_base64 ~password (String.concat lines)
