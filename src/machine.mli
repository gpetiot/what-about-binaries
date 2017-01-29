
type t;;

val of_int: int -> t;;
val pretty: Format.formatter -> t -> unit;;

type instr;;
val pretty_instr : Format.formatter -> instr -> unit;;
  
module type Instr =
sig
  val decode : Capstone.mode list -> string -> instr list
end;;

val instr: t -> (module Instr);;
