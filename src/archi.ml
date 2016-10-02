
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
  type t
  type int_t
  val eclass : eclass
  val size : int
  val header_size : int
  val inc : t -> t
  val pretty : Format.formatter -> t -> unit
  val of_list : int list -> t
  val logor : int_t -> int_t -> int_t
  val logand : int_t -> int_t -> int_t
  val shift_left : int_t -> int -> int_t
  val shift_right : int_t -> int -> int_t
  val shift_right_logical : int_t -> int -> int_t
  val of_int : int -> int_t
  val to_int : int_t -> int
  val ph_offsets : int * int * int * int * int * int * int * int
  val ph_sizes : int * int * int * int * int * int * int * int
end;;
    
module Addr32 =
struct
  type t = int * int * int * int
  type int_t = Int32.t
  let eclass = C32
  let size = 4
  let header_size = 52
  let inc (e,f,g,h) =
    if h < 255 then e,f,g,h+1
    else if g < 255 then e,f,g+1,0
    else if f < 255 then e,f+1,0,0
    else if e < 255 then e+1,0,0,0
    else assert false
  let pretty fmt (a,b,c,d) =
    Format.fprintf fmt "%a%a%a%a"
      Hexa.pretty a Hexa.pretty b Hexa.pretty c Hexa.pretty d
  let of_list = function
    | [a;b;c;d] -> a,b,c,d
    | _ -> failwith "Archi.Addr32.of_list"
  let logor = Int32.logor
  let logand = Int32.logand
  let shift_left = Int32.shift_left
  let shift_right = Int32.shift_right
  let shift_right_logical = Int32.shift_right_logical
  let of_int = Int32.of_int
  let to_int = Int32.to_int
  let ph_offsets = 0,4,8,12,16,20,24,28
  let ph_sizes = 4,4,4,4,4,4,4,4
end;;

module Addr64 =
struct
  type t = int * int * int * int * int * int * int * int
  type int_t = Int64.t
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
  let pretty fmt (a,b,c,d,e,f,g,h) =
    Format.fprintf fmt "%a%a%a%a%a%a%a%a"
      Hexa.pretty a Hexa.pretty b Hexa.pretty c Hexa.pretty d
      Hexa.pretty e Hexa.pretty f Hexa.pretty g Hexa.pretty h
  let of_list = function
    | [a;b;c;d;e;f;g;h] -> a,b,c,d,e,f,g,h
    | _ -> failwith "Archi.Addr64.of_list"
  let logor = Int64.logor
  let logand = Int64.logand
  let shift_left = Int64.shift_left
  let shift_right = Int64.shift_right
  let shift_right_logical = Int64.shift_right_logical
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let ph_offsets = 0,8,16,24,32,40,4,48
  let ph_sizes = 4,8,8,8,8,8,4,8
end;;

let addr = function
  | C32 -> (module Addr32 : Addr)
  | C64 -> (module Addr64 : Addr)
;;
