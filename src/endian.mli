
type t;;

val of_int: int -> t;;
val pretty: Format.formatter -> t -> unit;;

module type T =
sig
  val edata : t
  val order : 'a list -> 'a list
end;;

val endianness: t -> (module T);;
