(** Hashindex support. *)

(** A KV represents the type of keys and values stored in a hashindex.
 * Each has a fixed length (the key is assumed to be a hash).  The
 * get_data function should extract the data from the given Cstruct.t
 * at the given offset (and shouldn't access any data beyond offset +
 * data_len. *)
module type KV = sig
  type data
  val key_len : int
  val data_len : int

  val get_data : Cstruct.t -> int -> data
end

(** Implement Hashindex for a given stored data type. *)
module type INDEX = sig
  (** The type of the data returned.  Will match the data type in the
   * KV. *)
  type data

  (** The type of an open index file. *)
  type t

  (** Open an index from a given filename.  Will raise an exception if
    * it is unable to open the index. *)
  val of_filename : string -> t

  (** Look up a given key in the index, and return it if it is
   * present. *)
  val find : t -> key:Cstruct.t -> data option

  (** Pring some information about the hashindex. *)
  val dump_info : t -> unit

  (** Iterate over all of the active keys in the index. *)
  val iter : t -> f:(Cstruct.t -> unit) -> unit
end

(** Build an index for a particular Data type. *)
module Make_index (Data : KV) : INDEX with type data = Data.data
