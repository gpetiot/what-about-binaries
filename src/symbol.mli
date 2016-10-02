
module Visibility : sig
  type t
  val of_int : int -> t
  val pretty : Format.formatter -> t -> unit
end

module Ndx : sig
  type t
  val of_int : int -> t
  val pretty : Format.formatter -> t -> unit
end
    
module Binding : sig
  type t
  val of_int : int -> t
  val pretty : Format.formatter -> t -> unit
end;;

module Type : sig
  type t
  val of_int : int -> t
  val pretty : Format.formatter -> t -> unit
end;;
