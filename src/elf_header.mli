
exception Invalid_Elf;;

val parse_class_endianness : string -> Elf_types.eclass * Elf_types.endianness;;

module Make (A : Elf_types.Addr) (E : Elf_types.Endianness) :
(sig
  val parse : Elf_types.eclass -> Elf_types.endianness -> string ->
    Elf_types.elf_header
    module Strtab : sig
      val parse : string -> off:int -> size:int -> string
      val get : string -> int -> string
    end
    module Sh : sig
      val parse : Elf_types.elf_header -> string -> Elf_types.sh_entry list
      val get : string -> Elf_types.sh_entry list -> Elf_types.sh_entry
      val strtab : filename:string -> tablename:string ->
	Elf_types.sh_entry list	-> string
    end
    module Ph : sig
      val parse : Elf_types.elf_header -> string -> Elf_types.ph_entry list
    end
    module Symtbl : sig
      val parse : filename:string -> tablename:string -> strtab:string ->
	Elf_types.sh_entry list -> Elf_types.symtbl_entry list
      val get : string -> Elf_types.symtbl_entry list -> Elf_types.symtbl_entry
    end
    module Decode : sig
      val decode : filename:string -> secname:string -> Elf_types.emachine ->
	Elf_types.sh_entry list -> Elf_types.symtbl_entry list ->
	Elf_types.instr list
    end
end);;
