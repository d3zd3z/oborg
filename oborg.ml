(* Playing with oborg. *)

open Core

module Hash = Mirage_crypto.Hash
module PB = Pbkdf.Make (Hash.SHA256)
module AESCTR = Mirage_crypto.Cipher_block.AES.CTR

module Unpacker = struct
  let check_key iref name =
    if Option.is_some !iref then failwith ("Duplicate key: " ^ name)

  let set_int iref ~name (v : Msgpck.t) =
    check_key iref name;
    match v with
      | Int x -> iref := Some x
      | _ -> failwith "Invalid type for int"

  let set_int32 iref ~name (v : Msgpck.t) =
    check_key iref name;
    match v with
      | Int x -> iref := Some (Int32.of_int_exn x)
      | _ -> failwith "Invalid type for int"

  let set_string iref ~name (v : Msgpck.t) =
    check_key iref name;
    match v with
      | String x | Bytes x -> iref := Some x
      | _ -> failwith "Invalid type for string"

  let set_bool iref ~name (v : Msgpck.t) =
    check_key iref name;
    match v with
      | Bool x -> iref := Some x
      | _ -> failwith "Invalid type for bool"
end

module Key = struct
  open Unpacker

  type t = {
    version : int;
    chunk_seed : int32;
    enc_hmac_key : string;
    enc_key : string;
    id_key : string;
    repository_id : string;
    tam_required : bool;
  }
  [@@deriving show]

  let of_kvs (kvs : (Msgpck.t * Msgpck.t) list) =
    let version = ref None in
    let chunk_seed = ref None in
    let enc_hmac_key = ref None in
    let enc_key = ref None in
    let id_key = ref None in
    let repository_id = ref None in
    let tam_required = ref None in
    List.iter kvs ~f:(fun (k, v) -> match k with
      | String "version" -> set_int ~name:"version" version v
      | String "chunk_seed" -> set_int32 ~name:"chunk_seed" chunk_seed v
      | String "enc_hmac_key" -> set_string ~name:"enc_hmac_key" enc_hmac_key v
      | String "enc_key" -> set_string ~name:"enc_key" enc_key v
      | String "id_key" -> set_string ~name:"id_key" id_key v
      | String "repository_id" -> set_string ~name:"repository_id" repository_id v
      | String "tam_required" -> set_bool ~name:"tam_required" tam_required v
      | _ -> failwith "Invalid key in map");
    { version = Option.value_exn !version;
      chunk_seed = Option.value_exn !chunk_seed;
      enc_hmac_key = Option.value_exn !enc_hmac_key;
      enc_key = Option.value_exn !enc_key;
      id_key = Option.value_exn !id_key;
      repository_id = Option.value_exn !repository_id;
      tam_required = Option.value_exn !tam_required; }

  let of_string (msg : Msgpck.t) = match msg with
    | Map kvs -> of_kvs kvs
    | _ -> failwith "Invalid packed message"
end

module KeyWrap = struct
  open Unpacker

  type alg = SHA256
  [@@deriving show]

  type t = {
    version : int;
    salt : string;
    iterations : int;
    algorithm : alg;
    hash : string;
    data : string;
  } [@@deriving show]

  let of_kvs (kvs : (Msgpck.t * Msgpck.t) list) =
    let version = ref None in
    let salt = ref None in
    let iterations = ref None in
    let algorithm = ref None in
    let hash = ref None in
    let data = ref None in
    List.iter kvs ~f:(fun (k, v) -> match k with
      | String "version" -> set_int ~name:"version" version v
      | String "salt" -> set_string ~name:"salt" salt v
      | String "iterations" -> set_int ~name:"iterations" iterations v
      | String "algorithm" ->
          begin match v with
            | String "sha256" ->
                check_key algorithm "algorithm";
                algorithm := Some SHA256
            | _ -> failwith "Invalid algorithm"
          end
      | String "hash" -> set_string ~name:"hash" hash v
      | String "data" -> set_string ~name:"data" data v
      | _ -> failwith "Invalid key in map");
    { version = Option.value_exn !version;
      salt = Option.value_exn !salt;
      iterations = Option.value_exn !iterations;
      algorithm = Option.value_exn !algorithm;
      hash = Option.value_exn !hash;
      data = Option.value_exn !data }

  let of_string (msg : Msgpck.t) = match msg with
    | Map kvs -> of_kvs kvs
    | _ -> failwith "Invalid packed msg"

  let decode t pass =
    let password = Cstruct.of_string pass in
    let salt = Cstruct.of_string t.salt in
    let buf = PB.pbkdf2 ~password ~salt ~count:t.iterations ~dk_len:32l in

    let data = Cstruct.of_string t.data in
    let key = AESCTR.of_secret buf in
    let plain = AESCTR.decrypt ~key ~ctr:(0L,0L) data in
    printf "plain\n";
    Pdump.Cstruct.pdump plain;

    (* Compute hmac to validate. *)
    let digest = Hash.SHA256.hmac ~key:buf plain in
    let exp_digest = Cstruct.of_string t.hash in
    printf "exp digest\n";
    Pdump.Cstruct.pdump exp_digest;
    printf "digest\n";
    Pdump.Cstruct.pdump digest;

    if not (Cstruct.equal exp_digest digest) then
      failwith "Digest mismatch on key, invalid passphrase?";
    Cstruct.to_string plain
end

let showmap pairs =
  List.iter pairs ~f:(fun (k, d) ->
    printf "k: %S, v: %S\n" (Msgpck.show k) (Msgpck.show d))

(* Split the line based on the key. *)
let splitit line =
  match String.split line ~on:' ' with
    | [key; repoid] -> key, repoid
    | _ -> failwith "Invalid first line"

(* Try reading keys. *)
let load_key name =
  let lines = In_channel.read_lines name in
  let key, repoid, lines = match lines with
    | (x::xs) ->
        let key, repoid = splitit x in
        (key, repoid, xs)
    | _ -> failwith "No lines present in input" in
  printf "key: %S, repo: %S\n" key repoid;
  let bin = Base64.decode_exn (String.concat lines) in
  printf "data: %d bytes\n" (String.length bin);
  Pdump.String.pdump bin;

  let pos, payload = Msgpck.String.read bin in
  printf "pos: 0x%x\n" pos;

  let wrap = KeyWrap.of_string payload in
  printf "wrap = %s\n" (KeyWrap.show wrap);

  let plain = KeyWrap.decode wrap "kao3ohBae0quaMohzu5eemaeghei3Gox8zu" in
  let pos, plain = Msgpck.String.read plain in
  printf "pos: 0x%x\n" pos;
  printf "inner = %S\n" (Msgpck.show plain);
  let keyinfo = Key.of_string plain in
  printf "inner = %s\n" (Key.show keyinfo)

  (* List.iter lines ~f:(fun line -> printf "line: %S\n" line) *)

let () = load_key "/home/davidb/.config/borg/keys/lint_borgs_test"
