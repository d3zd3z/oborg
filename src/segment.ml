(* Reading of segment files.  The segment files are memory mapped, and
 * subsections of them will be shared. *)

open Core

module SHA256 = Mirage_crypto.Hash.SHA256
module AESCTR = Mirage_crypto.Cipher_block.AES.CTR

(* Each segment file consists of
 * ofs len desc
 * --- --- ----
 * 0   8   magic value "BORG_SEG"
 * 8       log entries
 *
 * Each log entry consts of
 * 0   4   le-u32 length (total, includes the header)
 * 4   4   crc32 of entry (includes payload)
 * 8   1   tag
 * 9   32  hash (for PUT and DELETE)
 * 41  ... encrypted payload
 *
 * The tag values are
 * 0   put    - both hash and encrypted payload
 * 1   delete - only hash
 * 2   commit - No hash and no payload
 *
 * A correct and current index file captures the latest put value that
 * hasn't been deleted, but has been committed.
 *)

type t = Cstruct.t

let of_filename path =
  let fd = Unix.openfile path ~mode:[Unix.O_RDONLY] in
  let data = Unix_cstruct.of_fd fd in
  Unix.close fd;
  let magic = Cstruct.copy data 0 8 in
  if String.(magic <> "BORG_SEG") then
    failwith "Invalid magic on segment file";
  data

(* Read a segment item.  This must be a 'put'. *)
let read t offset =
  let len = Int32.to_int_exn (Cstruct.LE.get_uint32 t (offset + 4)) in
  printf "len=0x%x\n" len;
  (* TODO check CRC *)
  (* let crc = Cstruct.LE.get_uint32 t offset in *)
  let code = Cstruct.get_uint8 t (offset + 9) in
  printf "code: 0x%02x\n" code;
  if code <> 0 then
    failwith "Index points to non-put entity";
  (* TODO: Compare the hash. *)
  let base = offset + 41 in
  Cstruct.sub t base (len - 41)

(* The payload itself
 * ofs len desc
 * --- --- ----
 * 0   1   type
 * 1   32  MAC
 * 33  8   IV
 * 41  ... ciphertext
 *)

(* Decrypt, given info from the repo config. *)
let decrypt payload (key : Keyfile.Key.t) =
  let _ttype = Cstruct.get_uint8 payload 0 in
  let mac = Cstruct.sub payload 1 32 in
  let iv = Cstruct.BE.get_uint64 payload 33 in
  printf "IV: 0x%Lx\n" iv;
  (* let iv = Cstruct.sub payload 33 8 in *)
  let cipher = Cstruct.sub payload 41 (Cstruct.len payload - 41) in
  let ivcipher = Cstruct.sub payload 33 (Cstruct.len payload - 33) in

  (* Validate the hmac.  The hmac is computed over the iv || cipher. *)
  let digest = SHA256.hmac ~key:key.enc_hmac_key ivcipher in

  if not (Cstruct.equal mac digest) then begin
    Cstruct.hexdump mac;
    Cstruct.hexdump digest;
    failwith "Digest mismatch"
  end;

  let key = AESCTR.of_secret key.enc_key in
  let plaintext = AESCTR.decrypt ~key ~ctr:(0L, iv) cipher in
  Cstruct.hexdump plaintext
