
type eclass;;

val of_int: int -> eclass;;
val pretty: Format.formatter -> eclass -> unit;;
  
module type Addr =
sig
  type t
  type int_t
  val eclass : eclass
  val size : int
  val header_size : int
  val inc : t -> t
  val pretty : Format.formatter -> t -> unit
  val of_list : int list -> t
  val logor : int_t -> int_t -> int_t
  val logand : int_t -> int_t -> int_t
  val shift_left : int_t -> int -> int_t
  val shift_right : int_t -> int -> int_t
  val shift_right_logical : int_t -> int -> int_t
  val of_int : int -> int_t
  val to_int : int_t -> int
  val ph_offsets : int * int * int * int * int * int * int * int
  val ph_sizes : int * int * int * int * int * int * int * int
end;;

val addr: eclass -> (module Addr);;
