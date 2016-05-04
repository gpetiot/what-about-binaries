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
  | None -> "none"
  | SPARC -> "sparc"
  | X86 -> "x86"
  | MIPS -> "mips"
  | PowerPC -> "powerpc"
  | ARM -> "arm"
  | SuperH -> "superh"
  | IA64 -> "ia64"
  | X86_64 -> "x86_64"
  | AArch64 -> "aarch64"
;;

let pretty fmt x = Format.fprintf fmt "%s" (to_string x);;
