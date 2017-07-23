
val class_endianness : string -> Elf_types.eclass * Elf_types.endianness;;

val elf_header : Elf_types.eclass_conf -> Elf_types.endianness ->
  string -> Elf_types.elf_header
;;
    
val section_header : Elf_types.endianness -> Elf_types.eclass_conf ->
  Elf_types.elf_header -> string -> Elf_types.sh_entry list
;;
  
val program_header : Elf_types.endianness -> Elf_types.eclass_conf ->
  Elf_types.elf_header -> string -> Elf_types.ph_entry list
;;
  
val symtbl : filename:string -> symtbl_name:string -> strtab_name:string ->
  Elf_types.sh_entry list -> Elf_types.endianness -> Elf_types.eclass_conf ->
  Elf_types.symbol list
;;

(* unused *)
val progbits_section : filename:string ->
  Elf_types.sh_entry -> Elf_types.elf_header -> Elf_types.eclass_conf ->
  (int * Elf_types.capstone_instr list) list
;;
  
val functions : filename:string -> secname:string -> Elf_types.elf_header ->
  Elf_types.eclass_conf -> Elf_types.sh_entry list ->
  Elf_types.symbol list -> Elf_types.funct list
;;
