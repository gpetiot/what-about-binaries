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

type eclass;;

val from_int : int -> eclass;;
val to_string : eclass -> string;;
  
module type Addr =
sig
  type t
  type int_t
  val eclass : eclass
  val size : int
  val header_size : int
  val inc : t -> t
  val to_string : t -> string
  val from_list : int list -> t
  val logor : int_t -> int_t -> int_t
  val shift_left : int_t -> int -> int_t
  val of_int : int -> int_t
  val to_int : int_t -> int
end;;

module Addr32 : Addr;;
module Addr64 : Addr;;

val addr: eclass -> (module Addr);;
