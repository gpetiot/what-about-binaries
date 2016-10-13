
type eclass;;

val of_int: int -> eclass;;
val pretty: Format.formatter -> eclass -> unit;;
  
module type Addr =
sig
  val eclass : eclass
  val size : int
  val header_size : int
  val ph_offsets : int * int * int * int * int * int * int * int
  val ph_sizes : int * int * int * int * int * int * int * int
  val sym_offsets : int * int * int * int * int * int
  val sym_sizes : int * int * int * int * int * int
end;;

val addr: eclass -> (module Addr);;
