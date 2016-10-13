
type eclass = C32 | C64;;

let of_int = function
  | 1 -> C32
  | 2 -> C64
  | x -> failwith (Format.sprintf "Archi.of_int %i" x)
;;

let pretty fmt = function
  | C32 -> Format.fprintf fmt "32"
  | C64 -> Format.fprintf fmt "64"
;;

module type Addr =
sig
  val eclass : eclass
  val size : int
  val header_size : int
  val ph_offsets : int * int * int * int * int * int * int * int
  val ph_sizes : int * int * int * int * int * int * int * int
  val sym_offsets : int * int * int * int * int * int
  val sym_sizes : int * int * int * int * int * int
end;;

module Addr32 =
struct
  let eclass = C32
  let size = 4
  let header_size = 52
  let word_size = 4
  let addr_size  = 4
  let half_size = 2
  let xword_size = 4
  let off_size = 4

  let ph_sizes =
    word_size, (* type *)
    off_size,  (* offset *)
    addr_size, (* vaddr *)
    addr_size, (* paddr *)
    word_size, (* filesz *)
    word_size, (* memsz *)
    word_size, (* flags *)
    word_size  (* align *)
    
  let ph_offsets =
    let ty, off, vaddr, paddr, filesz, memsz, flags, _align = ph_sizes in
    0,                                    (* type *)
    ty,                                   (* offset *)
    ty+off,                               (* vaddr *)
    ty+off+vaddr,                         (* paddr *)
    ty+off+vaddr+paddr,                   (* filesz *)
    ty+off+vaddr+paddr+filesz,            (* memsz *)
    ty+off+vaddr+paddr+filesz+memsz,      (* flags *)
    ty+off+vaddr+paddr+filesz+memsz+flags (* align *)
      
  let sym_sizes =
    word_size, (* name *)
    addr_size, (* value *)
    word_size, (* size *)
    1,         (* info *)
    1,         (* other *)
    half_size  (* shndex *)
    
  let sym_offsets =
    let name, value, size, info, other, _shndex = sym_sizes in
    0,                         (* name *)
    name,                      (* value *)
    name+value,                (* size *)
    name+value+size,           (* info *)
    name+value+size+info,      (* other *)
    name+value+size+info+other (* shndex *)
end;;

module Addr64 =
struct
  let eclass = C64
  let size = 8
  let header_size = 64
  let word_size = 4
  let addr_size  = 8
  let half_size = 2
  let xword_size = 8
  let off_size = 8
    
  let ph_sizes =
    word_size,  (* type *)
    off_size,   (* offset *)
    addr_size,  (* vaddr *)
    addr_size,  (* paddr *)
    xword_size, (* filesz *)
    xword_size, (* memsz *)
    word_size,  (* flags *)
    xword_size  (* align *)
      
  let ph_offsets =
    let ty, off, vaddr, paddr, filesz, memsz, flags, _align = ph_sizes in
    0,                                    (* type *)
    ty+flags,                             (* offset *)
    ty+flags+off,                         (* vaddr *)
    ty+flags+off+vaddr,                   (* paddr *)
    ty+flags+off+vaddr+paddr,             (* filesz *)
    ty+flags+off+vaddr+paddr+filesz,      (* memsz *)
    ty,                                   (* flags *)
    ty+flags+off+vaddr+paddr+filesz+memsz (* align *)
      
  let sym_sizes =
    word_size,  (* name *)
    addr_size,  (* value *)
    xword_size, (* size *)
    1,          (* info *)
    1,          (* other *)
    half_size   (* shndex *)

  let sym_offsets =
    let name, value, _size, info, other, shndex = sym_sizes in
    0,                            (* name *)
    name+info+other+shndex,       (* value *)
    name+info+other+shndex+value, (* size *)
    name,                         (* info *)
    name+info,                    (* other *)
    name+info+other               (* shndex *)
end;;

let addr = function
  | C32 -> (module Addr32 : Addr)
  | C64 -> (module Addr64 : Addr)
;;
