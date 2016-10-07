
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
  val word_size : int
  val addr_size : int
  val half_size : int
  val xword_size : int
  val off_size : int
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
  let ph_offsets = 0,4,8,12,16,20,24,28
  let ph_sizes = 4,4,4,4,4,4,4,4
  let sym_offsets =
    0,
    word_size,
    word_size+addr_size,
    2*word_size + addr_size,
    2*word_size + addr_size + 1,
    2*word_size + addr_size + 2
  let sym_sizes =
    word_size,
    addr_size,
    word_size,
    1,
    1,
    half_size
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
  let off_size = 4
  let ph_offsets = 0,8,16,24,32,40,4,48
  let ph_sizes = 4,8,8,8,8,8,4,8
  let sym_offsets =
    0,
    word_size+2+half_size,
    word_size+2+half_size+addr_size,
    word_size,
    word_size+1,
    word_size+2
  let sym_sizes =
    word_size,
    addr_size,
    xword_size,
    1,
    1,
    half_size
end;;

let addr = function
  | C32 -> (module Addr32 : Addr)
  | C64 -> (module Addr64 : Addr)
;;
