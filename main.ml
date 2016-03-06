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

type eclass = C32 | C64;;

let int_to_eclass = function
  | 1 -> C32
  | 2 -> C64
  | _ -> failwith "int_to_eclass"
;;

let eclass_to_string = function
  | C32 -> "32"
  | C64 -> "64"
;;

module type Addr =
sig
  type t
  val eclass : eclass
  val size : int
  val header_size : int
  val inc : t -> t
  val to_string : t -> string
  val from_list : int list -> t
end;;

module Addr32 : Addr =
struct
  type t = int * int * int * int
  let eclass = C32
  let size = 4
  let header_size = 52
  let inc (e,f,g,h) =
    if h < 255 then e,f,g,h+1
    else if g < 255 then e,f,g+1,0
    else if f < 255 then e,f+1,0,0
    else if e < 255 then e+1,0,0,0
    else assert false
  let to_string (a,b,c,d) =
    Printf.sprintf "0x%s%s%s%s"
      (Hexa.to_string a) (Hexa.to_string b) (Hexa.to_string c)(Hexa.to_string d)
  let from_list = function
    | [a;b;c;d] -> a,b,c,d
    | _ -> failwith "from_list"
end;;

module Addr64 =
struct
  type t = int * int * int * int * int * int * int * int
  let eclass = C64
  let size = 8
  let header_size = 64
  let inc (a,b,c,d,e,f,g,h) =
    if h < 255 then a,b,c,d,e,f,g,h+1
    else if g < 255 then a,b,c,d,e,f,g+1,0
    else if f < 255 then a,b,c,d,e,f+1,0,0
    else if e < 255 then a,b,c,d,e+1,0,0,0
    else if d < 255 then a,b,c,d+1,0,0,0,0
    else if c < 255 then a,b,c+1,0,0,0,0,0
    else if b < 255 then a,b+1,0,0,0,0,0,0
    else if a < 255 then a+1,0,0,0,0,0,0,0
    else assert false
  let to_string (a,b,c,d,e,f,g,h) =
    Printf.sprintf "0x%s%s%s%s%s%s%s%s"
      (Hexa.to_string a) (Hexa.to_string b) (Hexa.to_string c)(Hexa.to_string d)
      (Hexa.to_string e) (Hexa.to_string f) (Hexa.to_string g)(Hexa.to_string h)
  let from_list = function
    | [a;b;c;d;e;f;g;h] -> a,b,c,d,e,f,g,h
    | _ -> failwith "from_list"
end;;

type edata = LittleEndian | BigEndian;;

let int_to_edata = function
  | 1 -> LittleEndian
  | 2 -> BigEndian
  | _ -> failwith "int_to_edata"
;;

let edata_to_string = function
  | LittleEndian -> "little-endian"
  | BigEndian -> "big-endian"
;;

module type Endianness =
sig
  val edata : edata
  val order : 'a list -> 'a list
end;;

module BigEndianness =
struct
  let edata = BigEndian
  let order x = x
end;;

module LittleEndianness =
struct
  let edata = LittleEndian
  let order x = List.rev x
end;;

type etype = Relocatable | Executable | Shared | Core;;

let int_to_etype = function
  | 1 -> Relocatable
  | 2 -> Executable
  | 3 -> Shared
  | 4 -> Core
  | _ -> failwith "int_to_etype"
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
  else failwith "int_to_emachine"
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


module File_header (A : Addr) (E : Endianness) = struct
  type t = {
    ei_version : int;
    ei_osabi : int; (* ignored *)
    ei_abiversion : int; (* ignored *)
    ei_type : etype; (* ignored *)
    ei_machine : emachine;
    ei_entry : A.t;
    ei_phoff : int;
    ei_shoff : int;
    ei_ehsize : int;
    ei_phentsize : int;
    ei_phnum : int;
    ei_shentsize : int;
    ei_shnum : int;
    ei_shstrndx : int;
  };;

  let make version osabi abiversion etype emachine entry phoff
      shoff ehsize phentsize phnum shentsize shnum shstrndx = {
    ei_version = version;
    ei_osabi = osabi;
    ei_abiversion = abiversion;
    ei_type = etype;
    ei_machine = emachine;
    ei_entry = entry;
    ei_phoff = phoff;
    ei_shoff = shoff;
    ei_ehsize = ehsize;
    ei_phentsize = phentsize;
    ei_phnum = phnum;
    ei_shentsize = shentsize;
    ei_shnum = shnum; 
    ei_shstrndx = shstrndx;
  };;

  let print e =
    Printf.printf
      "class: %s bits\ndata: %s\nversion: %i\netype: %s\nmachine: %s\n\
entry: %s\nphoff: %i\nshoff: %i\nehsize: %i\nphentsize: %i\nphnum: %i\n\
shentsize: %i\nshnum: %i\nshstrndx: %i\n"
      (eclass_to_string A.eclass)
      (edata_to_string E.edata)
      e.ei_version
      (etype_to_string e.ei_type)
      (emachine_to_string e.ei_machine)
      (A.to_string e.ei_entry)
      e.ei_phoff
      e.ei_shoff
      e.ei_ehsize
      e.ei_phentsize
      e.ei_phnum
      e.ei_shentsize
      e.ei_shnum
      e.ei_shstrndx
  ;;

  let multi_bytes buf off nb =
    let rec aux ret soff =
      match soff with
      | x when x = nb -> ret
      | x -> aux ((Buffer.nth buf (off+x))::ret) (soff+1)
    in
    let l = List.rev_map int_of_char (aux [] 0) in
    E.order l
  ;;

  let multi_bytes_int buf off nb  =
    List.fold_left (fun x y -> x*255+y) 0 (multi_bytes buf off nb)
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
      let ehsize = multi_bytes_int buf (24+A.size*3+4) 2 in
      assert (ehsize = A.header_size);
      let phentsize = multi_bytes_int buf (24+A.size*3+6) 2 in
      let phnum = multi_bytes_int buf (24+A.size*3+8) 2 in
      let shentsize = multi_bytes_int buf (24+A.size*3+10) 2 in
      let shnum = multi_bytes_int buf (24+A.size*3+12) 2 in
      let shstrndx = multi_bytes_int buf (24+A.size*3+14) 2 in
      close_in chan;
      make version osabi abiversion etype emachine
	entry phoff shoff ehsize phentsize phnum shentsize shnum shstrndx
    with exn ->
      close_in chan;
      Printf.printf "%s" (Printexc.to_string exn);
      raise Invalid_Elf
  ;;

  let parse header filename =
    let chan = open_in_bin filename in
    let rec aux chan i addr =
      try
	let bint = input_byte chan in
	if i >= header.ei_ehsize + header.ei_phentsize * header.ei_phnum
	  && i < header.ei_shoff
	then
	  (Printf.printf
	    "%i: %s: \t %s\n" i (A.to_string addr) (Hexa.to_string bint);
	   aux chan (i+1) (A.inc addr))
	else
	  aux chan (i+1) addr
      with
      | End_of_file -> close_in chan
      | exn ->
	 close_in chan;
	Printf.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    in
    aux chan 0 header.ei_entry;
    print header
  ;;

  let start filename =
    let fh = parse_header filename in
    parse fh filename
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
    int_to_eclass (int_of_char (Buffer.nth buf 4)),
    int_to_edata (int_of_char (Buffer.nth buf 5))
  with exn ->
    close_in chan;
    Printf.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;


let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = parse_class_endianness filename in
	let addr = match eclass with
	  | C32 -> (module Addr32 : Addr)
	  | C64 -> (module Addr64 : Addr)
	in
	let endianness = match edata with
	  | LittleEndian -> (module LittleEndianness : Endianness)
	  | BigEndian -> (module BigEndianness : Endianness)
	in
	let module A = (val addr) in
	let module E = (val endianness) in
	let module FH = File_header(A)(E) in
	FH.start filename
      with
	Invalid_Elf -> Printf.printf "invalid ELF file !\n"
    else
      Printf.printf "%s does not exist !\n" filename
  else
    Printf.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
