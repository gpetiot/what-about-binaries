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

exception Invalid_Elf;;

val parse_class_endianness : string -> Archi.eclass * Endian.t;;

module Make (A : Archi.Addr) (E : Endian.T) : (sig
  type t
  type sh_entry
  type ph_entry
  val print_sh_entry : sh_entry -> unit
  val print_ph_entry : ph_entry -> unit
  val print : t -> unit
  val parse_header : string -> t
  val parse_section_header : t -> string -> sh_entry list
  val parse_program_header : t -> string -> ph_entry list
end);;
