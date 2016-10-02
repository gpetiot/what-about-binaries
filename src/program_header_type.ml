
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
  
let pretty fmt x = Format.fprintf fmt "%-14s" (to_string x);;
  
