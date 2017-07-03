
let char_hexa = function
  | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4'
  | 5 -> '5' | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9'
  | 10 -> 'a' | 11 -> 'b' | 12 -> 'c' | 13 -> 'd' | 14 -> 'e' | 15 -> 'f'
  | x -> failwith (Format.sprintf "Print.char_hexa %i" x)
;;

let hexa fmt i =
  let rec aux n p inc = function
    | [] -> n, p
    | pow2 :: t ->
       aux (n mod pow2) (if n / pow2 = 1 then p + inc else p) (inc / 2) t
  in
  let j, p1 = aux i 0 8 [128; 64; 32; 16] in
  let _, p2 = aux j 0 8 [8; 4; 2; 1] in
  Format.fprintf fmt "%c%c" (char_hexa p1) (char_hexa p2)
;;

open Elf_types

let endianness fmt = function
  | LittleEndian -> Format.fprintf fmt "little endian"
  | BigEndian -> Format.fprintf fmt "big endian"
;;

let to_string = function
  | TNone -> "NONE"
  | Relocatable -> "RELOCATABLE"
  | Executable -> "EXEC (Executable file)"
  | Shared -> "SHARED"
  | Core -> "CORE"
  | Loproc -> "LOPROC"
  | Hiproc -> "HIPROC"
;;
let etype fmt x = Format.fprintf fmt "%s" (to_string x);;

let to_string : Elf_types.ph_type -> string = function
  | Null -> "NULL"
  | Load -> "LOAD"
  | Dynamic -> "DYNAMIC"
  | Interp -> "INTERP"
  | Note -> "NOTE"
  | Shlib -> "SHLIB"
  | Phdr -> "PHDR"
  | Tls -> "TLS"
  | Loos -> "LOOS"
  | Amdgpu_hsa_load_global_agent -> "AMDGPU_HSA_LOAD_GLOBAL_AGENT"
  | Amdgpu_hsa_load_readonly_agent -> "AMDGPU_HSA_LOAD_READONLY_AGENT"
  | Amdgpu_hsa_load_code_agent -> "AMDGPU_HSA_LOAD_CODE_AGENT"
  | Sunw_unwind -> "SUNW_UNWIND"
  | Gnu_eh_frame -> "GNU_EH_FRAME"
  | Gnu_stack -> "GNU_STACK"
  | Gnu_relro -> "GNU_RELRO"
  | Hios -> "HIOS"
  | Loproc -> "LOPROC"
  | Arm_exidx -> "EXIDX"
  | Mips_options -> "MIPS_OPTIONS"
  | Mips_abiflags -> "MIPS_ABIFLAGS"
  | Hiproc -> "HIPROC"
;;
let ph_type fmt x = Format.fprintf fmt "%-14s" (to_string x);;
  
let to_string = function
  | Null -> "NULL"
  | Progbits -> "PROGBITS"
  | Symtab -> "SYMTAB"
  | Strtab -> "STRTAB"
  | Rela -> "RELA"
  | Hash -> "HASH"
  | Dynamic -> "DYNAMIC"
  | Note -> "NOTE"
  | Nobits -> "NOBITS"
  | Rel -> "REL"
  | Shlib -> "SHLIB"
  | Dynsym -> "DYNSYM"
  | Init_array -> "INIT_ARRAY"
  | Fini_array -> "FINI_ARRAY"
  | Preinit_array -> "PREINIT_ARRAY"
  | Group -> "GROUP"
  | Symtab_shndx -> "SYMTAB_SHNDX"
  | Loos -> "LOOS"
  | Gnu_attributes -> "GNU_ATTRIBUTES"
  | Gnu_hash -> "GNU_HASH"
  | Gnu_verdef -> "VERDEF"
  | Gnu_verneed -> "VERNEED"
  | Gnu_versym -> "VERSYM"
  | Loproc -> "LOPROC"
  | Arm_exidx -> "ARM_EXIDX"
  | Arm_preemptmap -> "ARM_PREEMPTMAP"
  | Arm_attributes -> "ARM_ATTRIBUTES"
  | Arm_debugoverlay -> "ARM_DEBUGOVERLAY"
  | Arm_overlaysection -> "ARM_OVERLAYSECTION"
  | Mips_reginfo -> "MIPS_REGINFO"
  | Mips_options -> "MIPS_OPTIONS"
  | Mips_abiflags -> "MIPS_ABIFLAGS"
  | Hiproc -> "HIPROC"
  | Louser -> "LOUSER"
  | Hiuser -> "HIUSER"
;;
let sh_type fmt x = Format.fprintf fmt "%-15s" (to_string x);;

let to_string = function
  | Default -> "DEFAULT"
  | Hidden -> "HIDDEN"
;;
let sym_visibility fmt x = Format.fprintf fmt "%-8s" (to_string x);;

let to_string = function
  | Abs -> "ABS"
  | Und -> "UND"
  | Int x -> string_of_int x
;;
let sym_ndx fmt x = Format.fprintf fmt "%3s" (to_string x);;

let to_string = function
  | Local -> "LOCAL"
  | Global -> "GLOBAL"
  | Weak -> "WEAK"
;;
let sym_binding fmt x = Format.fprintf fmt "%-6s" (to_string x);;

let to_string = function
  | Notype -> "NOTYPE"
  | Object -> "OBJECT"
  | Func -> "FUNC"
  | Section -> "SECTION"
  | File -> "FILE"
;;
let sym_type fmt x = Format.fprintf fmt "%-7s" (to_string x);;

let to_string = function
  | MNone -> "NONE"
  | SPARC -> "SPARC"
  | X86 -> "X86"
  | MIPS -> "MIPS"
  | PowerPC -> "POWERPC"
  | ARM -> "ARM"
  | SuperH -> "SUPERH"
  | IA64 -> "IA64"
  | X86_64 -> "X86_64"
  | AArch64 -> "AARCH64"
;;
let emachine fmt x = Format.fprintf fmt "%s" (to_string x);;

let instr fmt insn =
  Format.fprintf fmt "0x%x\t%s\t%s\n"
    insn.Capstone.address insn.Capstone.mnemonic insn.Capstone.op_str;
;;

let eclass fmt = function
  | C32 -> Format.fprintf fmt "32"
  | C64 -> Format.fprintf fmt "64"
;;

let elf_header fmt e =
  Format.fprintf
    fmt
    "\
\  Class:                             ELF%a\n\
\  Data:                              %a\n\
\  Version:                           %i\n\
\  OS/ABI:                            %i\n\
\  ABI Version:                       %i\n\
\  Type:                              %a\n\
\  Machine:                           %a\n\
\  Entry point address:               0x%x\n\
\  Start of program headers:          %i (bytes into file)\n\
\  Start of section headers:          %i (bytes into file)\n\
\  Flags:                             0x%x\n\
\  Size of this header:               %i (bytes)\n\
\  Size of program headers:           %i (bytes)\n\
\  Number of program headers:         %i\n\
\  Size of section headers:           %i (bytes)\n\
\  Number of section headers:         %i\n\
\  Section header string table index: %i\n"
    eclass e.ei_class
    endianness e.ei_endian
    e.ei_version
    e.ei_osabi
    e.ei_abiversion
    etype e.ei_type
    emachine e.ei_machine
    e.ei_entry
    e.ei_phoff
    e.ei_shoff
    e.ei_flags
    e.ei_ehsize
    e.ei_phentsize
    e.ei_phnum
    e.ei_shentsize
    e.ei_shnum
    e.ei_shstrndx
;;

let flag_read f = (f land 4) <> 0;;
let flag_write f = (f land 2) <> 0;;
let flag_exec f = (f land 1) <> 0;;

let ph_entry fmt e =
  Format.fprintf
    fmt
    "  %a 0x%016x 0x%016x 0x%016x 0x%x 0x%x %c%c%c %x\n"
    ph_type e.ph_type
    e.ph_off
    e.ph_vaddr
    e.ph_addr
    e.ph_filesz
    e.ph_memsz
    (if flag_read e.ph_flags then 'R' else ' ')
    (if flag_write e.ph_flags then 'W' else ' ')
    (if flag_exec e.ph_flags then 'E' else ' ')
    e.ph_align
;;

let sh_entry fmt e =
  Format.fprintf
    fmt
    "%-17s %a %08x %08x %08x %02x %3i %2i %3i %2i\n"
    e.sh_name
    sh_type e.sh_type
    e.sh_addr
    e.sh_off
    e.sh_size
    e.sh_entsize
    e.sh_flags
    e.sh_link
    e.sh_info
    e.sh_addralign
;;

let symtbl_entry fmt x =
  Format.fprintf
    fmt "%08x  %4i %a %a %a %a %s\n"
    x.value
    x.size
    sym_type x.symtype
    sym_binding x.bind
    sym_visibility x.vis
    sym_ndx x.ndx
    x.name
;;
