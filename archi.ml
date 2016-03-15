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

type eclass = C32 | C64;;

let from_int = function
  | 1 -> C32
  | 2 -> C64
  | x -> failwith (Format.sprintf "from_int %i" x)
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
  val from_list : int list -> t
  val logor : int_t -> int_t -> int_t
  val shift_left : int_t -> int -> int_t
  val of_int : int -> int_t
  val to_int : int_t -> int
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
    Format.fprintf fmt "0x%a%a%a%a"
      Hexa.pretty a Hexa.pretty b Hexa.pretty c Hexa.pretty d
  let from_list = function
    | [a;b;c;d] -> a,b,c,d
    | _ -> failwith "from_list"
  let logor = Int32.logor
  let shift_left = Int32.shift_left
  let of_int = Int32.of_int
  let to_int = Int32.to_int
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
    Format.fprintf fmt "0x%a%a%a%a%a%a%a%a"
      Hexa.pretty a Hexa.pretty b Hexa.pretty c Hexa.pretty d
      Hexa.pretty e Hexa.pretty f Hexa.pretty g Hexa.pretty h
  let from_list = function
    | [a;b;c;d;e;f;g;h] -> a,b,c,d,e,f,g,h
    | _ -> failwith "from_list"
  let logor = Int64.logor
  let shift_left = Int64.shift_left
  let of_int = Int64.of_int
  let to_int = Int64.to_int
end;;

let addr = function
  | C32 -> (module Addr32 : Addr)
  | C64 -> (module Addr64 : Addr)
;;
