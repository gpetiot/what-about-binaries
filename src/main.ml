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
	let pp_sep fmt () = Format.fprintf fmt "" in

	(* file header *)
	let fh = FH.parse filename in
	Format.printf "file header:\n%a" FH.pretty fh;

	(* section header *)
	let shl = FH.Sh.parse fh filename in
	Format.printf
	  "\nsection header:\n%a"
	  (Format.pp_print_list ~pp_sep FH.Sh.pretty) shl;

	(* program header *)
	let phl = FH.Ph.parse fh filename in
	Format.printf
	  "\nprogram header:\n%a"
	  (Format.pp_print_list ~pp_sep FH.Ph.pretty) phl;

	(* symbol table *)
	let strtab = FH.Sh.strtab ~filename ~tablename:".strtab" shl in
	let symtab =
	  FH.Symtbl.parse ~filename ~tablename:".symtab" ~strtab shl in
	Format.printf
	  "\nsymbol table:\n%a"
	  (Format.pp_print_list ~pp_sep FH.Symtbl.pretty) symtab;
	
	(* dynamic symbol table *)
	begin
	  try
	    let dstrtab = FH.Sh.strtab ~filename ~tablename:".dynstr" shl in
	    let dsymtab =
	      FH.Symtbl.parse ~filename ~tablename:".dynsym" ~strtab:dstrtab shl
	    in
	    Format.printf
	      "\ndynamic symbol table:\n%a"
	      (Format.pp_print_list ~pp_sep FH.Symtbl.pretty) dsymtab
	  with Not_found -> ()
	end
      with
	Elf_header.Invalid_Elf -> Format.printf "invalid ELF file !\n"
    else
      Format.printf "%s does not exist !\n" filename
  else
    Format.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
