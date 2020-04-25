(** Hashindex support. *)

module Index : sig
  type t
  val dump_info : t -> unit
  val of_filename : string -> t

  val iter : t -> f:(Cstruct.t -> unit) -> unit
end
