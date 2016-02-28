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
  val to_string : t -> string
  val from_buffer : Buffer.t -> int -> t
end;;

module Addr32 : Addr =
struct
  type t = int * int * int * int
  let eclass = C32
  let size = 4
  let to_string (a,b,c,d) =
    Printf.sprintf "0x%s%s%s%s"
      (Hexa.to_string a) (Hexa.to_string b) (Hexa.to_string c)(Hexa.to_string d)
  let from_buffer buf off =
    int_of_char (Buffer.nth buf off), int_of_char (Buffer.nth buf (off+1)),
    int_of_char (Buffer.nth buf (off+2)), int_of_char (Buffer.nth buf (off+3))
end;;

module Addr64 =
struct
  type t = int * int * int * int * int * int * int * int
  let eclass = C64
  let size = 8
  let to_string (a,b,c,d,e,f,g,h) =
    Printf.sprintf "0x%s%s%s%s%s%s%s%s"
      (Hexa.to_string a) (Hexa.to_string b) (Hexa.to_string c)(Hexa.to_string d)
      (Hexa.to_string e) (Hexa.to_string f) (Hexa.to_string g)(Hexa.to_string h)
  let from_buffer buf off =
    int_of_char (Buffer.nth buf off), int_of_char (Buffer.nth buf (off+1)),
    int_of_char (Buffer.nth buf (off+2)), int_of_char (Buffer.nth buf (off+3)),
    int_of_char (Buffer.nth buf (off+4)), int_of_char (Buffer.nth buf (off+5)),
    int_of_char (Buffer.nth buf (off+6)), int_of_char (Buffer.nth buf (off+7))
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


module File_header (A : Addr) = struct
  type t = {
    ei_class : eclass;
    ei_data : edata;
    ei_version : int;
    ei_osabi : int; (* ignored *)
    ei_abiversion : int; (* ignored *)
    ei_type : etype; (* ignored *)
    ei_machine : emachine;
    ei_entry : A.t;
    ei_phoff : A.t;
    ei_shoff : A.t;
    ei_ehsize : int;
    ei_phentsize : int;
    ei_phnum : int;
    ei_shentsize : int;
    ei_shnum : int;
    ei_shstrndx : int;
  };;

  let make eclass edata version osabi abiversion etype emachine entry phoff
      shoff ehsize phentsize phnum shentsize shnum shstrndx = {
    ei_class = eclass;
    ei_data = edata;
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
entry: %s\nphoff: %s\nshoff: %s\nehsize: %i\nphentsize: %i\nphnum: %i\n\
shentsize: %i\nshnum: %i\nshstrndx: %i\n"
      (eclass_to_string e.ei_class)
      (edata_to_string e.ei_data)
      e.ei_version
      (etype_to_string e.ei_type)
      (emachine_to_string e.ei_machine)
      (A.to_string e.ei_entry)
      (A.to_string e.ei_phoff)
      (A.to_string e.ei_shoff)
      e.ei_ehsize
      e.ei_phentsize
      e.ei_phnum
      e.ei_shentsize
      e.ei_shnum
      e.ei_shstrndx
  ;;

  let parse header_size filename =
    let chan = open_in_bin filename in
    let buf = Buffer.create header_size in
    try
      Buffer.add_channel buf chan header_size;
      let edata = int_to_edata (int_of_char (Buffer.nth buf 5)) in
      let version = int_of_char (Buffer.nth buf 6) in
      let osabi = int_of_char (Buffer.nth buf 7) in
      let abiversion = int_of_char (Buffer.nth buf 8) in
      let etype = int_to_etype (int_of_char (Buffer.nth buf 16)) in
      assert (int_of_char (Buffer.nth buf 17) = 0);
      let emachine = int_to_emachine (int_of_char (Buffer.nth buf 18)) in
      assert (int_of_char (Buffer.nth buf 19) = 0);
      assert (int_of_char (Buffer.nth buf 20) = version);
      assert (int_of_char (Buffer.nth buf 21) = 0);
      assert (int_of_char (Buffer.nth buf 22) = 0);
      assert (int_of_char (Buffer.nth buf 23) = 0);
      let entry = A.from_buffer buf 24 in
      let phoff = A.from_buffer buf (24+A.size) in
      let shoff = A.from_buffer buf (24+A.size*2) in
      let ehsize = int_of_char (Buffer.nth buf (24+A.size*3+4)) in
      assert (int_of_char (Buffer.nth buf (24+A.size*3+5)) = 0);
      let phentsize = int_of_char (Buffer.nth buf (24+A.size*3+6)) in
      assert (int_of_char (Buffer.nth buf (24+A.size*3+7)) = 0);
      let phnum = int_of_char (Buffer.nth buf (24+A.size*3+8)) in
      assert (int_of_char (Buffer.nth buf (24+A.size*3+9)) = 0);
      let shentsize = int_of_char (Buffer.nth buf (24+A.size*3+10)) in
      assert (int_of_char (Buffer.nth buf (24+A.size*3+11)) = 0);
      let shnum = int_of_char (Buffer.nth buf (24+A.size*3+12)) in
      assert (int_of_char (Buffer.nth buf (24+A.size*3+13)) = 0);
      let shstrndx = int_of_char (Buffer.nth buf (24+A.size*3+14)) in
      assert (int_of_char (Buffer.nth buf (24+A.size*3+15)) = 0);
      let fh = make A.eclass edata version osabi abiversion etype emachine
	entry phoff shoff ehsize phentsize phnum shentsize shnum shstrndx in
      print fh;
      close_in chan
    with _ ->
      close_in chan;
      raise Invalid_Elf
  ;;
end;;


let parse_class filename =
  let chan = open_in_bin filename in
  let buf = Buffer.create 5 in
  try
    Buffer.add_channel buf chan 5;
    assert (Buffer.nth buf 0 = '\x7F');
    assert (Buffer.nth buf 1 = '\x45');
    assert (Buffer.nth buf 2 = '\x4c');
    assert (Buffer.nth buf 3 = '\x46');
    int_to_eclass (int_of_char (Buffer.nth buf 4))
  with _ ->
    close_in chan;
    raise Invalid_Elf
;;


let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass = parse_class filename in
	match eclass with
	| C32 ->
	   let module FH = File_header(Addr32) in
	   FH.parse 52 filename
	| C64 ->
	   let module FH = File_header(Addr64) in
	   FH.parse 64 filename
      with
	Invalid_Elf -> Printf.printf "invalid ELF file !\n"
    else
      Printf.printf "%s does not exist !\n" filename
  else
    Printf.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
