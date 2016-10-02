
type shtype =
  | Null
  | Progbits
  | Symtab
  | Strtab
  | Rela
  | Hash
  | Dynamic
  | Note
  | Nobits
  | Rel
  | Shlib
  | Dynsym
  | Init_array
  | Fini_array
  | Preinit_array
  | Group
  | Symtab_shndx
  | Loos
  | Gnu_attributes
  | Gnu_hash
  | Gnu_verdef
  | Gnu_verneed
  | Gnu_versym (* or Hios *)
  | Loproc (* or Hex_ordered *)
  | Arm_exidx (* or X86_64_unwind *)
  | Arm_preemptmap
  | Arm_attributes
  | Arm_debugoverlay
  | Arm_overlaysection
  | Mips_reginfo
  | Mips_options
  | Mips_abiflags
  | Hiproc
  | Louser
  | Hiuser
;;

type t = shtype;;

let of_int = function
  | 0 -> Null
  | 1 -> Progbits
  | 2 -> Symtab
  | 3 -> Strtab
  | 4 -> Rela
  | 5 -> Hash
  | 6 -> Dynamic
  | 7 -> Note
  | 8 -> Nobits
  | 9 -> Rel
  | 10 -> Shlib
  | 11 -> Dynsym
  | 14 -> Init_array
  | 15 -> Fini_array
  | 16 -> Preinit_array
  | 17 -> Group
  | 18 -> Symtab_shndx
  | 1610612736 -> Loos
  | 1879048181 -> Gnu_attributes
  | 1879048182 -> Gnu_hash
  | 1879048189 -> Gnu_verdef
  | 1879048190 -> Gnu_verneed
  | 1879048191 -> Gnu_versym
  | 1879048192 -> Loproc
  | 1879048193 -> Arm_exidx
  | 1879048194 -> Arm_preemptmap
  | 1879048195 -> Arm_attributes
  | 1879048196 -> Arm_debugoverlay
  | 1879048197 -> Arm_overlaysection
  | 1879048198 -> Mips_reginfo
  | 1879048205 -> Mips_options
  | 1879048234 -> Mips_abiflags
  | 2147483647 -> Hiproc
  | 2147483648 -> Louser
  | 4294967295 -> Hiuser
  | x -> failwith (Format.sprintf "Section_header_type.of_int %i" x)
;;

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
  
let pretty fmt x = Format.fprintf fmt "%-15s" (to_string x);;
  
