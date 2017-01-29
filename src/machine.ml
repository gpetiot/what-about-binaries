
type emachine =
  | None
  | SPARC
  | X86
  | MIPS
  | PowerPC
  | ARM
  | SuperH
  | IA64
  | X86_64
  | AArch64
;;

type t = emachine;;
type instr = Capstone.cs_insn0;;

let of_int i =
  if i = int_of_string "0x00" then None
  else if i = int_of_string "0x02" then SPARC
  else if i = int_of_string "0x03" then X86
  else if i = int_of_string "0x08" then MIPS
  else if i = int_of_string "0x14" then PowerPC
  else if i = int_of_string "0x28" then ARM
  else if i = int_of_string "0x2A" then SuperH
  else if i = int_of_string "0x32" then IA64
  else if i = int_of_string "0x3E" then X86_64
  else if i = int_of_string "0xB7" then AArch64
  else failwith (Format.sprintf "Machine.of_int %i" i)
;;
  
let to_string = function
  | None -> "NONE"
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

let pretty fmt x = Format.fprintf fmt "%s" (to_string x);;


module type Instr =
sig
  val decode : Capstone.mode list -> string -> instr list
end;;


let pretty_instr fmt insn =
  Format.fprintf fmt "0x%x\t%s\t%s\n"
    insn.Capstone.address insn.Capstone.mnemonic insn.Capstone.op_str;
;;

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
  | None
  | SPARC
  | MIPS
  | PowerPC
  | SuperH
  | IA64
  | AArch64 ->
     failwith (Format.sprintf "No instruction set for %s" (to_string x))
;;
