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

type etype = Relocatable | Executable | Shared | Core;;

let int_to_etype = function
  | 1 -> Relocatable
  | 2 -> Executable
  | 3 -> Shared
  | 4 -> Core
  | x -> failwith (Format.sprintf "int_to_etype %i" x)
;;

let etype_to_string = function
  | Relocatable -> "relocatable"
  | Executable -> "executable"
  | Shared -> "shared"
  | Core -> "core"
;;

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

let int_to_emachine i =
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
  else failwith (Format.sprintf "int_to_emachine %i" i)
;;

let emachine_to_string = function
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

type shtype =
  | Null
  | Progbits
  | Symtab
  | Strtab
  | Rela
  | Dynamic
  | Note
  | Nobits
  | Rel
  | Dynsym
  | Init_array
  | Fini_array
  | Gnu_hash
  | Verneed
  | Versym
  | Arm_exidx
  | Arm_attributes
;;

let int_to_shtype = function
  | 0 -> Null
  | 1 -> Progbits
  | 2 -> Symtab
  | 3 -> Strtab
  | 4 -> Rela
  | 6 -> Dynamic
  | 7 -> Note
  | 8 -> Nobits
  | 9 -> Rel
  | 11 -> Dynsym
  | 14 -> Init_array
  | 15 -> Fini_array
  | 1879048182 -> Gnu_hash
  | 1879048190 -> Verneed
  | 1879048191 -> Versym
  | 1879048193 -> Arm_exidx
  | 1879048195 -> Arm_attributes
  | x -> failwith (Format.sprintf "int_to_shtype %i" x)
;;

let shtype_to_string = function
  | Null -> "null"
  | Progbits -> "progbits"
  | Symtab -> "symtab"
  | Strtab -> "strtab"
  | Rela -> "rela"
  | Dynamic -> "dynamic"
  | Note -> "note"
  | Nobits -> "nobits"
  | Rel -> "rel"
  | Dynsym -> "dynsym"
  | Init_array -> "init_array"
  | Fini_array -> "fini_array"
  | Gnu_hash -> "gnu_hash"
  | Verneed -> "verneed"
  | Versym -> "versym"
  | Arm_exidx -> "arm_exidx"
  | Arm_attributes -> "arm_attributes"
;;


module File_header (A : Archi.Addr) (E : Endian.T) = struct
  type t = {
    ei_version : int;
    ei_osabi : int; (* ignored *)
    ei_abiversion : int; (* ignored *)
    ei_type : etype; (* ignored *)
    ei_machine : emachine;
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

  type sh_entry = {
    sh_name : int;
    sh_type : shtype;
    sh_flags : int;
    sh_addr : A.t;
    sh_off : int;
    sh_size : int;
  };;

  let print_sh_entry e =
    Format.printf "name: %i; type: %s; flags: %i; addr: %a; off: %i; size: %i\n"
      e.sh_name (shtype_to_string e.sh_type) e.sh_flags A.pretty e.sh_addr
      e.sh_off e.sh_size
  ;;

  let make version osabi abiversion etype emachine entry phoff
      shoff flags ehsize phentsize phnum shentsize shnum shstrndx = {
    ei_version = version;
    ei_osabi = osabi;
    ei_abiversion = abiversion;
    ei_type = etype;
    ei_machine = emachine;
    ei_entry = entry;
    ei_phoff = phoff;
    ei_shoff = shoff;
    ei_flags = flags;
    ei_ehsize = ehsize;
    ei_phentsize = phentsize;
    ei_phnum = phnum;
    ei_shentsize = shentsize;
    ei_shnum = shnum; 
    ei_shstrndx = shstrndx;
  };;

  let print e =
    Format.printf
      "class: %a bits\ndata: %a\nversion: %i\netype: %s\nmachine: %s\n\
       entry: %a\nphoff: %i\nshoff: %i\nflags: %i\nehsize: %i\nphentsize: %i\n\
       phnum: %i\nshentsize: %i\nshnum: %i\nshstrndx: %i\n"
      Archi.pretty A.eclass
      Endian.pretty E.edata
      e.ei_version
      (etype_to_string e.ei_type)
      (emachine_to_string e.ei_machine)
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
    A.from_list (multi_bytes buf off nb)
  ;;
  
  let parse_header filename =
    let chan = open_in_bin filename in
    let buf = Buffer.create A.header_size in
    try
      Buffer.add_channel buf chan A.header_size;
      let version = int_of_char (Buffer.nth buf 6) in
      let osabi = int_of_char (Buffer.nth buf 7) in
      let abiversion = int_of_char (Buffer.nth buf 8) in
      let etype = int_to_etype (multi_bytes_int buf 16 2) in
      let emachine = int_to_emachine (multi_bytes_int buf 18 2) in
      let version' = multi_bytes_int buf 20 4 in
      assert (version = version');
      let entry = multi_bytes_addr buf 24 A.size in
      let phoff = multi_bytes_int buf (24+A.size) A.size in
      let shoff = multi_bytes_int buf (24+A.size*2) A.size in
      let flags = multi_bytes_int buf (24+A.size*3) 4 in
      let ehsize = multi_bytes_int buf (28+A.size*3) 2 in
      assert (ehsize = A.header_size);
      let phentsize = multi_bytes_int buf (30+A.size*3) 2 in
      let phnum = multi_bytes_int buf (32+A.size*3) 2 in
      let shentsize = multi_bytes_int buf (34+A.size*3) 2 in
      let shnum = multi_bytes_int buf (36+A.size*3) 2 in
      let shstrndx = multi_bytes_int buf (38+A.size*3) 2 in
      close_in chan;
      make version osabi abiversion etype emachine entry phoff shoff flags
	   ehsize phentsize phnum shentsize shnum shstrndx
    with exn ->
      close_in chan;
      Format.printf "%s" (Printexc.to_string exn);
      raise Invalid_Elf
  ;;

  let parse_section_header header filename =
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
	    let sh_type = int_to_shtype (multi_bytes_int buf 4 4) in
	    let sh_flags = multi_bytes_int buf 8 A.size in
	    let sh_addr = multi_bytes_addr buf (8+A.size) A.size in
	    let sh_off = multi_bytes_int buf (8+A.size*2) A.size in
	    let sh_size = multi_bytes_int buf (8+A.size*3) A.size in
	    let sh = {sh_name; sh_type; sh_flags; sh_addr; sh_off; sh_size} in
	    aux chan (i+header.ei_shentsize) (sh::ret)
	  end
	else
	  ret
    in
    try
      let r = aux chan 0 [] in
      close_in chan;
      List.rev r
    with exn ->
      close_in chan;
      Format.printf "%s" (Printexc.to_string exn);
      raise Invalid_Elf
  ;;

  let parse_section_names shstrtab filename =
    let chan = open_in_bin filename in
    let rec aux chan i =
      if i < shstrtab.sh_off then
	let _ = input_byte chan in
	aux chan (i+1)
      else
	let buf = Buffer.create shstrtab.sh_size in
	Buffer.add_channel buf chan shstrtab.sh_size;
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
  
  let start filename =
    let fh = parse_header filename in
    let shl = parse_section_header fh filename in
    print fh;
    let shstrtab = List.nth shl fh.ei_shstrndx in
    let shnames = parse_section_names shstrtab filename in
    List.iter (fun entry ->
      let i = entry.sh_name in
      let name = get_section_name shnames i in
      Format.printf "section %s:\n" name;
      print_sh_entry entry
    ) shl;
  ;;
end;;


let parse_class_endianness filename =
  let chan = open_in_bin filename in
  let buf = Buffer.create 6 in
  try
    Buffer.add_channel buf chan 6;
    assert (Buffer.nth buf 0 = '\x7F');
    assert (Buffer.nth buf 1 = '\x45');
    assert (Buffer.nth buf 2 = '\x4c');
    assert (Buffer.nth buf 3 = '\x46');
    Archi.from_int (int_of_char (Buffer.nth buf 4)),
    Endian.from_int (int_of_char (Buffer.nth buf 5))
  with exn ->
    close_in chan;
    Format.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;


let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = parse_class_endianness filename in
	let module A = (val (Archi.addr eclass)) in
	let module E = (val (Endian.endianness edata)) in
	let module FH = File_header(A)(E) in
	FH.start filename
      with
	Invalid_Elf -> Format.printf "invalid ELF file !\n"
    else
      Format.printf "%s does not exist !\n" filename
  else
    Format.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
