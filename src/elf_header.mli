
exception Invalid_Elf;;

val parse_class_endianness : string -> Elf_types.eclass * Elf_types.endianness;;

val parse_elf_header : Elf_types.eclass_conf -> Elf_types.endianness ->
  string -> Elf_types.elf_header
;;
    
val parse_section_header : Elf_types.endianness -> Elf_types.eclass_conf ->
  Elf_types.elf_header -> string -> Elf_types.sh_entry list
;;

val strtab : filename:string -> tablename:string ->
  Elf_types.sh_entry list -> Elf_types.endianness -> string
;;
  
val parse_program_header : Elf_types.endianness -> Elf_types.eclass_conf ->
  Elf_types.elf_header -> string -> Elf_types.ph_entry list
;;
  
val parse_symtbl : filename:string -> tablename:string -> strtab:string ->
  Elf_types.sh_entry list -> Elf_types.endianness -> Elf_types.eclass_conf ->
  Elf_types.symtbl_entry list
;;
  
val decode_instr : filename:string -> secname:string -> Elf_types.elf_header ->
  Elf_types.eclass_conf -> Elf_types.sh_entry list ->
  Elf_types.symtbl_entry list -> Elf_types.instr list
;;
