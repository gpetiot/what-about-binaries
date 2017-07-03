
open Elf_types

module Arm_Instr : Instr =
struct
  let arch = Capstone.CS_ARCH_ARM

  let decode _ _ = [];;
end;;

module X86_Instr : Instr =
struct
  let arch = Capstone.CS_ARCH_X86
    
  let decode modes str =
    let handle = Capstone.cs_open arch modes in
    let _=
      Capstone.cs_option handle Capstone.CS_OPT_DETAIL Capstone._CS_OPT_ON in
    let instr = Capstone.cs_disasm handle str 0x1000L 0L in
    (match Capstone.cs_close handle with
    | 0 -> ()
    | _ -> failwith "Failed to close handle");
    instr
  ;;
;;
end;;

let instr x = match x with
  | ARM -> (module Arm_Instr : Instr)
  | X86
  | X86_64 -> (module X86_Instr : Instr)
  | MNone
  | SPARC
  | MIPS
  | PowerPC
  | SuperH
  | IA64
  | AArch64 ->
     failwith (Format.sprintf "No instruction set for instruction set")
;;
