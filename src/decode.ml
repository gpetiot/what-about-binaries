
open Elf_types

let etype = function
  | 0 -> TNone
  | 1 -> Relocatable
  | 2 -> Executable
  | 3 -> Shared
  | 4 -> Core
  | 65280 -> Loproc
  | 65535 -> Hiproc
  | x -> failwith (Format.sprintf "Decode.etype %i" x)
;;

let eclass = function
  | 1 -> C32
  | 2 -> C64
  | x -> failwith (Format.sprintf "Decode.eclass %i" x)
;;

let ph_type = function
  | 0 -> (Null : ph_type)
  | 1 -> Load
  | 2 -> Dynamic
  | 3 -> Interp
  | 4 -> Note
  | 5 -> Shlib
  | 6 -> Phdr
  | 7 -> Tls
  | 1610612736 -> Loos
  | 1610612737 -> Amdgpu_hsa_load_global_agent
  | 1610612738 -> Amdgpu_hsa_load_readonly_agent
  | 1610612739 -> Amdgpu_hsa_load_code_agent
  | 1684333904 -> Sunw_unwind
  | 1685382480 -> Gnu_eh_frame
  | 1685382481 -> Gnu_stack
  | 1685382482 -> Gnu_relro
  | 1879048191 -> Hios
  | 1879048192 -> Loproc
  | 1879048193 -> Arm_exidx
  | 1879048194 -> Mips_options
  | 1879048195 -> Mips_abiflags
  | 2147483647 -> Hiproc
  | x -> failwith (Format.sprintf "Decode.ph_type %i" x)
;;

let sh_type = function
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
  | x -> failwith (Format.sprintf "Decode.sh_type %i" x)
;;

let sym_visibility = function
  | 0 -> Default
  | 2 -> Hidden
  | x -> failwith (Format.sprintf "Decode.sym_visibility %i" x)
;;

let sym_ndx = function
  | 0 -> Und
  | 65521 -> Abs
  | x -> Int x
;;

let sym_binding = function
  | 0 -> Local
  | 1 -> Global
  | 2 -> Weak
  | x -> failwith (Format.sprintf "Decode.sym_binding %i" x)
;;

let sym_type = function
  | 0 -> Notype
  | 1 -> Object
  | 2 -> Func
  | 3 -> Section
  | 4 -> File
  | x -> failwith (Format.sprintf "Decode.sym_type %i" x)
;;

let endianness = function
  | 1 -> LittleEndian
  | 2 -> BigEndian
  | x -> failwith (Format.sprintf "Decode.endianness %i" x)
;;

let emachine i =
  if i = int_of_string "0x00" then MNone
  else if i = int_of_string "0x02" then SPARC
  else if i = int_of_string "0x03" then X86
  else if i = int_of_string "0x08" then MIPS
  else if i = int_of_string "0x14" then PowerPC
  else if i = int_of_string "0x28" then ARM
  else if i = int_of_string "0x2A" then SuperH
  else if i = int_of_string "0x32" then IA64
  else if i = int_of_string "0x3E" then X86_64
  else if i = int_of_string "0xB7" then AArch64
  else failwith (Format.sprintf "Decode.emachine %i" i)
;;
