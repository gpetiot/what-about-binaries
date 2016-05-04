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

exception Invalid_Elf

type etype = None | Relocatable | Executable | Shared | Core | Loproc | Hiproc;;

let int_to_etype = function
  | 0 -> None
  | 1 -> Relocatable
  | 2 -> Executable
  | 3 -> Shared
  | 4 -> Core
  | 65280 -> Loproc
  | 65535 -> Hiproc
  | x -> failwith (Format.sprintf "Elf_header.int_to_etype %i" x)
;;

let etype_to_string = function
  | None -> "none"
  | Relocatable -> "relocatable"
  | Executable -> "executable"
  | Shared -> "shared"
  | Core -> "core"
  | Loproc -> "loproc"
  | Hiproc -> "hiproc"
;;

let parse_class_endianness filename =
  let chan = open_in_bin filename in
  let buf = Buffer.create 6 in
  try
    Buffer.add_channel buf chan 6;
    assert (Buffer.nth buf 0 = '\x7F');
    assert (Buffer.nth buf 1 = '\x45');
    assert (Buffer.nth buf 2 = '\x4c');
    assert (Buffer.nth buf 3 = '\x46');
    Archi.of_int (int_of_char (Buffer.nth buf 4)),
    Endian.of_int (int_of_char (Buffer.nth buf 5))
  with exn ->
    close_in chan;
    Format.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;

module Make (A : Archi.Addr) (E : Endian.T) = struct
  type t = {
    ei_version : int;
    ei_osabi : int;
    ei_abiversion : int;
    ei_type : etype;
    ei_machine : Machine.t;
    ei_entry : A.t;
    ei_phoff : int;
    ei_shoff : int;
    ei_flags : int;
    ei_ehsize : int;
    ei_phentsize : int;
    ei_phnum : int;
    ei_shentsize : int;
    ei_shnum : int;
    ei_shstrndx : int;
  };;

  let pretty fmt e =
    Format.fprintf
      fmt
      "class: %a bits\ndata: %a\nversion: %i\netype: %s\nmachine: %a\n\
       entry: %a\nphoff: %i\nshoff: %i\nflags: %i\nehsize: %i\nphentsize: %i\n\
       phnum: %i\nshentsize: %i\nshnum: %i\nshstrndx: %i\n"
      Archi.pretty A.eclass
      Endian.pretty E.edata
      e.ei_version
      (etype_to_string e.ei_type)
      Machine.pretty e.ei_machine
      A.pretty e.ei_entry
      e.ei_phoff
      e.ei_shoff
      e.ei_flags
      e.ei_ehsize
      e.ei_phentsize
      e.ei_phnum
      e.ei_shentsize
      e.ei_shnum
      e.ei_shstrndx
  ;;

  let multi_bytes buf off nb =
    let rec aux ret = function
      | x when x = nb -> ret
      | x -> aux ((Buffer.nth buf (off+x))::ret) (x+1)
    in
    let l = List.rev_map int_of_char (aux [] 0) in
    E.order l
  ;;

  let multi_bytes_int buf off nb =
    let rec aux ret i = function
      | [] -> ret
      | h :: t ->
	 aux (A.logor ret (A.shift_left (A.of_int h) (8*(nb-i-1)))) (i+1) t
    in
    A.to_int (aux (A.of_int 0) 0 (multi_bytes buf off nb))
  ;;

  let multi_bytes_addr buf off nb =
    A.of_list (multi_bytes buf off nb)
  ;;
  
  let parse filename =
    let chan = open_in_bin filename in
    let buf = Buffer.create A.header_size in
    try
      Buffer.add_channel buf chan A.header_size;
      let ei_version = int_of_char (Buffer.nth buf 6) in
      let ei_osabi = int_of_char (Buffer.nth buf 7) in
      let ei_abiversion = int_of_char (Buffer.nth buf 8) in
      let ei_type = int_to_etype (multi_bytes_int buf 16 2) in
      let ei_machine = Machine.of_int (multi_bytes_int buf 18 2) in
      let version' = multi_bytes_int buf 20 4 in
      assert (ei_version = version');
      let ei_entry = multi_bytes_addr buf 24 A.size in
      let ei_phoff = multi_bytes_int buf (24+A.size) A.size in
      let ei_shoff = multi_bytes_int buf (24+A.size*2) A.size in
      let ei_flags = multi_bytes_int buf (24+A.size*3) 4 in
      let ei_ehsize = multi_bytes_int buf (28+A.size*3) 2 in
      assert (ei_ehsize = A.header_size);
      let ei_phentsize = multi_bytes_int buf (30+A.size*3) 2 in
      let ei_phnum = multi_bytes_int buf (32+A.size*3) 2 in
      let ei_shentsize = multi_bytes_int buf (34+A.size*3) 2 in
      let ei_shnum = multi_bytes_int buf (36+A.size*3) 2 in
      let ei_shstrndx = multi_bytes_int buf (38+A.size*3) 2 in
      close_in chan;
      { ei_version; ei_osabi; ei_abiversion; ei_type; ei_machine; ei_entry;
	ei_phoff; ei_shoff; ei_flags; ei_ehsize; ei_phentsize; ei_phnum;
	ei_shentsize; ei_shnum; ei_shstrndx }
    with exn ->
      close_in chan;
      Format.printf "%s" (Printexc.to_string exn);
      raise Invalid_Elf
  ;;

  module Ph = struct
    type entry = {
      ph_type : Program_header_type.t;
      ph_off : int;
      ph_vaddr : A.t;
      ph_addr : A.t;
      ph_filesz : int;
      ph_memsz : int;
      ph_flags : int;
      ph_align : int;
    };;

    let pretty fmt e =
      Format.fprintf
	fmt
	"type: %a; off: %i; vaddr: %a; addr: %a; filesz: %i; memsz: %i; \
	 flags: %i; align: %i\n"
	Program_header_type.pretty e.ph_type e.ph_off A.pretty e.ph_vaddr
	A.pretty e.ph_addr e.ph_filesz e.ph_memsz e.ph_flags
	e.ph_align
    ;;

    let parse header filename =
      let chan = open_in_bin filename in
      let rec aux chan i ret =
	let pbeg = header.ei_phoff in
	let pend = header.ei_phoff + header.ei_phentsize * header.ei_phnum in
	if i < pbeg then
	  let _ = input_byte chan in
	  aux chan (i+1) ret
	else
	  if i < pend then
	    begin
	      let buf = Buffer.create header.ei_phentsize in
	      Buffer.add_channel buf chan header.ei_phentsize;
	      let o1,o2,o3,o4,o5,o6,o7,o8 = A.ph_offsets in
	      let s1,s2,s3,s4,s5,s6,s7,s8 = A.ph_sizes in
	      let ph_type =
		Program_header_type.of_int (multi_bytes_int buf o1 s1) in
	      let ph_off = multi_bytes_int buf o2 s2 in
	      let ph_vaddr = multi_bytes_addr buf o3 s3 in
	      let ph_addr = multi_bytes_addr buf o4 s4 in
	      let ph_filesz = multi_bytes_int buf o5 s5 in
	      let ph_memsz = multi_bytes_int buf o6 s6 in
	      let ph_flags = multi_bytes_int buf o7 s7 in
	      let ph_align = multi_bytes_int buf o8 s8 in
	      let ph = { ph_type; ph_off; ph_vaddr; ph_addr; ph_filesz; ph_memsz;
			 ph_flags; ph_align } in
	      aux chan (i+header.ei_phentsize) (ph::ret)
	    end
	  else
	    ret
      in
      try
	let r = List.rev (aux chan 0 []) in
	close_in chan;
	r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;
  end;;

  module Sh = struct
    type entry = {
      sh_name : string;
      sh_type : Section_header_type.t;
      sh_flags : int;
      sh_addr : A.t;
      sh_off : int;
      sh_size : int;
      sh_link : int;
      sh_info : int;
      sh_addralign : int;
      sh_entsize : int;
    };;

    let pretty fmt e =
      Format.fprintf
	fmt
	"name: %s; type: %a; flags: %i; addr: %a; off: %i; size: %i; \
	 link: %i; info: %i; align: %i; entsize: %i\n"
	e.sh_name Section_header_type.pretty e.sh_type e.sh_flags
	A.pretty e.sh_addr
	e.sh_off e.sh_size e.sh_link e.sh_info e.sh_addralign e.sh_entsize
    ;;

    let parse_section_names off size filename =
      let chan = open_in_bin filename in
      let rec aux chan i =
	if i < off then
	  let _ = input_byte chan in
	  aux chan (i+1)
	else
	  let buf = Buffer.create size in
	  Buffer.add_channel buf chan size;
	  Buffer.contents buf
      in
      try
	let r = aux chan 0 in
	close_in chan;
	r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;

    let get_section_name snames index =
      let end_index = String.index_from snames index (char_of_int 0) in
      String.sub snames index (end_index - index)
    ;;

    let parse header filename =
      let chan = open_in_bin filename in
      let rec aux chan i ret =
	let sbeg = header.ei_shoff in
	let send = header.ei_shoff + header.ei_shentsize * header.ei_shnum in
	if i < sbeg then
	  let _ = input_byte chan in
	  aux chan (i+1) ret
	else
	  if i < send then
	    begin
	      let buf = Buffer.create header.ei_shentsize in
	      Buffer.add_channel buf chan header.ei_shentsize;
	      let sh_name = multi_bytes_int buf 0 4 in
	      let sh_type = Section_header_type.of_int(multi_bytes_int buf 4 4) in
	      let sh_flags = multi_bytes_int buf 8 A.size in
	      let sh_addr = multi_bytes_addr buf (8+A.size) A.size in
	      let sh_off = multi_bytes_int buf (8+A.size*2) A.size in
	      let sh_size = multi_bytes_int buf (8+A.size*3) A.size in
	      let sh_link = multi_bytes_int buf (8+A.size*4) 4 in
	      let sh_info = multi_bytes_int buf (12+A.size*4) 4 in
	      let sh_addralign = multi_bytes_int buf (16+A.size*4) A.size in
	      let sh_entsize = multi_bytes_int buf (16+A.size*5) A.size in
	      let sh = sh_name, sh_type, sh_flags, sh_addr, sh_off, sh_size,
		       sh_link, sh_info, sh_addralign, sh_entsize in
	      aux chan (i+header.ei_shentsize) (sh::ret)
	    end
	  else
	    ret
      in
      try
	let r = List.rev (aux chan 0 []) in
	close_in chan;
	let _,_,_,_,off,size,_,_,_,_ = List.nth r header.ei_shstrndx in
	let shnames = parse_section_names off size filename in
	let name_section (id,sh_type,sh_flags,sh_addr,sh_off,sh_size,sh_link,
			  sh_info,sh_addralign,sh_entsize) =
	  let sh_name = get_section_name shnames id in
	  { sh_name; sh_type; sh_flags; sh_addr; sh_off; sh_size; sh_link;
	    sh_info; sh_addralign; sh_entsize }
	in
	List.map name_section r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;
  end;;
end;;
