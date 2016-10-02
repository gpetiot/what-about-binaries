
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
