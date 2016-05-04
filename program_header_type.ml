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

type phtype =
  | Null
  | Load
  | Dynamic
  | Interp
  | Note
  | Shlib
  | Phdr
  | Tls
  | Loos (* or Amdgpu_hsa_load_global_program *)
  | Amdgpu_hsa_load_global_agent
  | Amdgpu_hsa_load_readonly_agent
  | Amdgpu_hsa_load_code_agent
  | Sunw_unwind
  | Gnu_eh_frame (* or Sunw_eh_frame *)
  | Gnu_stack
  | Gnu_relro
  | Hios
  | Loproc (* or Arm_archext or Mips_reginfo *)
  | Arm_exidx (* or Arm_unwind or Mips_rtproc *)
  | Mips_options
  | Mips_abiflags
  | Hiproc
;;
  
type t = phtype;;

let of_int = function
  | 0 -> Null
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
  | x -> failwith (Format.sprintf "Program_header_type.of_int %i" x)
;;

let to_string = function
  | Null -> "null"
  | Load -> "load"
  | Dynamic -> "dynamic"
  | Interp -> "interp"
  | Note -> "note"
  | Shlib -> "shlib"
  | Phdr -> "phdr"
  | Tls -> "tls"
  | Loos -> "loos"
  | Amdgpu_hsa_load_global_agent -> "amdgpu_hsa_load_global_agent"
  | Amdgpu_hsa_load_readonly_agent -> "amdgpu_hsa_load_readonly_agent"
  | Amdgpu_hsa_load_code_agent -> "amdgpu_hsa_load_code_agent"
  | Sunw_unwind -> "sunw_unwind"
  | Gnu_eh_frame -> "gnu_eh_frame"
  | Gnu_stack -> "gnu_stack"
  | Gnu_relro -> "gnu_relro"
  | Hios -> "hios"
  | Loproc -> "loproc"
  | Arm_exidx -> "arm_exidx"
  | Mips_options -> "mips_options"
  | Mips_abiflags -> "mips_abiflags"
  | Hiproc -> "hiproc"
;;
  
let pretty fmt x = Format.fprintf fmt "%s" (to_string x);;
  
