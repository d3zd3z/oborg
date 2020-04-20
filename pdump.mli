module type S = sig
  type t
  val pdump : t -> unit
end

module String : S with type t = string
module Cstruct : S with type t = Cstruct.t
