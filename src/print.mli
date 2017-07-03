
val hexa: Format.formatter -> int -> unit;;

val ph_type : Format.formatter -> Elf_types.ph_type -> unit;;
  
val sh_type : Format.formatter -> Elf_types.sh_type -> unit;;

val sym_visibility : Format.formatter -> Elf_types.sym_visibility -> unit;;

val sym_ndx : Format.formatter -> Elf_types.sym_ndx -> unit;;

val sym_binding : Format.formatter -> Elf_types.sym_binding -> unit;;

val sym_type : Format.formatter -> Elf_types.sym_type -> unit;;

val etype : Format.formatter -> Elf_types.etype -> unit;;

val endianness: Format.formatter -> Elf_types.endianness -> unit;;

val emachine : Format.formatter -> Elf_types.emachine -> unit;;

val instr : Format.formatter -> Elf_types.instr -> unit;;

val elf_header : Format.formatter -> Elf_types.elf_header -> unit;;

val eclass : Format.formatter -> Elf_types.eclass -> unit;;

val ph_entry : Format.formatter -> Elf_types.ph_entry -> unit;;

val sh_entry : Format.formatter -> Elf_types.sh_entry -> unit;;

val symtbl_entry : Format.formatter -> Elf_types.symtbl_entry -> unit;;
