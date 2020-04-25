(** Hashindex support. *)

module Index : sig
  type t
  val dump_info : t -> unit
  val of_filename : string -> t

  (** Try to lookup a hash in the table.  Will return None if not
   * present or Some (int * int) of the two values, when it is found. *)
  val find : t -> key:Cstruct.t -> (int * int) option

  (** For testing purposes, iterate over all of the hashes, that are
   * not marked empty or deleted. *)
  val iter : t -> f:(Cstruct.t -> unit) -> unit
end
