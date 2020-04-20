(* Playing with oborg. *)

open Core

module Hash = Mirage_crypto.Hash
module PB = Pbkdf.Make (Hash.SHA256)
module AESCTR = Mirage_crypto.Cipher_block.AES.CTR

module KeyWrap = struct
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

  let set_int iref (v : Msgpck.t) =
    if Option.is_some !iref then failwith "Duplicate key";
    match v with
      | Int x -> iref := Some x
      | _ -> failwith "Invalid type for int"

  let set_string iref (v : Msgpck.t) =
    if Option.is_some !iref then failwith "Duplicate key";
    match v with
      | String x | Bytes x -> iref := Some x
      | _ -> failwith "Invalid type fo string"

  let of_kvs (kvs : (Msgpck.t * Msgpck.t) list) =
    let version = ref None in
    let salt = ref None in
    let iterations = ref None in
    let algorithm = ref None in
    let hash = ref None in
    let data = ref None in
    List.iter kvs ~f:(fun (k, v) -> match k with
      | String "version" -> set_int version v
      | String "salt" -> set_string salt v
      | String "iterations" -> set_int iterations v
      | String "algorithm" ->
          begin match v with
            | String "sha256" ->
                if Option.is_some !algorithm then failwith "Duplicate key";
                algorithm := Some SHA256
            | _ -> failwith "Invalid algorithm"
          end
      | String "hash" -> set_string hash v
      | String "data" -> set_string data v
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
  printf "inner = %S\n" (Msgpck.show plain)

  (* List.iter lines ~f:(fun line -> printf "line: %S\n" line) *)

let () = load_key "/home/davidb/.config/borg/keys/lint_borgs_test"
