(**************************************************************************)
(*                                                                        *)
(*  This file is part of OBAF.                                            *)
(*                                                                        *)
(*  You can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.0.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3.0                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

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
  | Null -> "null"
  | Progbits -> "progbits"
  | Symtab -> "symtab"
  | Strtab -> "strtab"
  | Rela -> "rela"
  | Hash -> "hash"
  | Dynamic -> "dynamic"
  | Note -> "note"
  | Nobits -> "nobits"
  | Rel -> "rel"
  | Shlib -> "shlib"
  | Dynsym -> "dynsym"
  | Init_array -> "init_array"
  | Fini_array -> "fini_array"
  | Preinit_array -> "preinit_array"
  | Group -> "group"
  | Symtab_shndx -> "symtab_shndx"
  | Loos -> "loos"
  | Gnu_attributes -> "gnu_attributes"
  | Gnu_hash -> "gnu_hash"
  | Gnu_verdef -> "gnu_verdef"
  | Gnu_verneed -> "gnu_verneed"
  | Gnu_versym -> "gnu_versym"
  | Loproc -> "loproc"
  | Arm_exidx -> "arm_exidx"
  | Arm_preemptmap -> "arm_preemptmap"
  | Arm_attributes -> "arm_attributes"
  | Arm_debugoverlay -> "arm_debugoverlay"
  | Arm_overlaysection -> "arm_overlaysection"
  | Mips_reginfo -> "mips_reginfo"
  | Mips_options -> "mips_options"
  | Mips_abiflags -> "mips_abiflags"
  | Hiproc -> "hiproc"
  | Louser -> "louser"
  | Hiuser -> "hiuser"
;;
  
let pretty fmt x = Format.fprintf fmt "%s" (to_string x);;
  
