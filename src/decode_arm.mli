
val print_arch : Capstone.arch -> Capstone.mode list -> string -> unit

val decode : Capstone.mode list -> string -> Elf_types.instr list;;
