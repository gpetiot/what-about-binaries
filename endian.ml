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

type t = LittleEndian | BigEndian;;

let from_int = function
  | 1 -> LittleEndian
  | 2 -> BigEndian
  | x -> failwith (Printf.sprintf "int_to_edata %i" x)
;;

let to_string = function
  | LittleEndian -> "little-endian"
  | BigEndian -> "big-endian"
;;

module type T =
sig
  val edata : t
  val order : 'a list -> 'a list
end;;

module Big =
struct
  let edata = BigEndian
  let order x = x
end;;

module Little =
struct
  let edata = LittleEndian
  let order x = List.rev x
end;;

let endianness = function
  | LittleEndian -> (module Little : T)
  | BigEndian -> (module Big : T)
;;

