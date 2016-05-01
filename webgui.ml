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

let onload _ =
  let document = Dom_html.window##document in
  let err () = assert false in
  let main = Js.Opt.get (document##getElementById (Js.string "main")) err in
  let div = Dom_html.createDiv document in
  Dom.appendChild main div;
  let filename = "../tests/hello.arm" in
  if Sys.file_exists filename then
    try
      let eclass, edata = Elf_header.parse_class_endianness filename in
      let module A = (val (Archi.addr eclass)) in
      let module E = (val (Endian.endianness edata)) in
      let module FH = Elf_header.Make(A)(E) in
      let fh = FH.parse_header filename in
      let shl = FH.parse_section_header fh filename in
      let phl = FH.parse_program_header fh filename in
      begin
  	let buf = document##createDocumentFragment() in
  	Dom.appendChild
	  buf (document##createTextNode (Js.string "file header:\n"));
  	FH.print fh;
  	Format.printf "\nsection header:\n";
  	List.iter FH.print_sh_entry shl;
  	Format.printf "\nprogram header:\n";
  	List.iter FH.print_ph_entry phl;
  	Dom.appendChild div buf
      end
    with
      Elf_header.Invalid_Elf ->
	Dom_html.window##alert (Js.string "invalid ELF file !")
  else
    Dom_html.window##alert (Js.string (filename ^ " does not exist !"));
  Js._false
;;

let _ = Dom_html.window##onload <- Dom_html.handler onload;;
