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

val of_int: int -> eclass;;
val pretty: Format.formatter -> eclass -> unit;;
  
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
  val shift_left : int_t -> int -> int_t
  val of_int : int -> int_t
  val to_int : int_t -> int
  val ph_offsets : int * int * int * int * int * int * int * int
  val ph_sizes : int * int * int * int * int * int * int * int
end;;

val addr: eclass -> (module Addr);;
