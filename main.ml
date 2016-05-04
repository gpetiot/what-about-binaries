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

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = Elf_header.parse_class_endianness filename in
	let module A = (val (Archi.addr eclass)) in
	let module E = (val (Endian.endianness edata)) in
	let module FH = Elf_header.Make(A)(E) in
	let fh = FH.parse filename in
	let shl = FH.Sh.parse fh filename in
	let phl = FH.Ph.parse fh filename in
	begin
	  Format.printf "file header:\n%a" FH.pretty fh;
	  let pp_sep fmt () = Format.fprintf fmt "" in
	  Format.printf "\nsection header:\n%a"
			(Format.pp_print_list ~pp_sep FH.Sh.pretty) shl;
	  Format.printf "\nprogram header:\n%a"
			(Format.pp_print_list ~pp_sep FH.Ph.pretty) phl
	end
      with
	Elf_header.Invalid_Elf -> Format.printf "invalid ELF file !\n"
    else
      Format.printf "%s does not exist !\n" filename
  else
    Format.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
