(**
 * Borg stores keys in a wrapped, password-protected file.
 *)

module Key : sig
  type t = {
    version : int;
    chunk_seed : int32;
    enc_hmac_key : Cstruct.t;
    enc_key : Cstruct.t;
    id_key : Cstruct.t;
    repository_id : string;
    tam_required : bool;
  }

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

val load_key : password:Cstruct.t -> string -> Key.t
val from_base64 : password:Cstruct.t -> string -> Key.t
