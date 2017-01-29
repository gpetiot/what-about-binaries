
exception Invalid_Elf;;

val parse_class_endianness : string -> Archi.eclass * Endian.t;;

module Make (A : Archi.Addr) (E : Endian.T) :
(sig
    type t
    val pretty : Format.formatter -> t -> unit
    val parse : string -> t
    module Strtab : sig
      val parse : string -> off:int -> size:int -> string
      val get : string -> int -> string
    end
    module Sh : sig
      type entry
      val pretty : Format.formatter -> entry -> unit
      val parse : t -> string -> entry list
      val get : string -> entry list -> entry
      val offset : entry -> int
      val size : entry -> int
      val entry_size : entry -> int
      val strtab : filename:string -> tablename:string -> entry list
	-> string
    end
    module Ph : sig
      type entry
      val pretty : Format.formatter -> entry -> unit
      val parse : t -> string -> entry list
    end
    module Symtbl : sig
      type entry
      val pretty : Format.formatter -> entry -> unit
      val parse : filename:string -> tablename:string -> strtab:string ->
	Sh.entry list -> entry list
      val get : string -> entry list -> entry
      val value : entry -> int
      val size : entry -> int
    end
    val machine : t -> Machine.t
    module Decode : sig
      val decode : filename:string -> secname:string -> Machine.t ->
	Sh.entry list -> Symtbl.entry list -> Machine.instr list
    end
end);;
