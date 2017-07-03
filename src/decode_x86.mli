
val print_arch: Capstone.arch -> Capstone.mode list -> string -> Int64.t -> unit

val decode : Capstone.mode list -> string -> Elf_types.instr list;;
