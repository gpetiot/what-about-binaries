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

module Make (A : Archi.Addr) (E : Endian.T) :
(sig
    type t
    val pretty : Format.formatter -> t -> unit
    val parse : string -> t
    module Strtab : sig
      val parse : string -> off:int -> size:int -> string
      val get : string -> int -> string
    end
    module Sh : sig
      type entry
      val pretty : Format.formatter -> entry -> unit
      val parse : t -> string -> entry list
      val get : string -> entry list -> entry
      val offset : entry -> int
      val size : entry -> int
      val entry_size : entry -> int
      val strtab : filename:string -> tablename:string -> entry list
	-> string
    end
    module Ph : sig
      type entry
      val pretty : Format.formatter -> entry -> unit
      val parse : t -> string -> entry list
    end
    module Symtbl : sig
      type entry
      val pretty : Format.formatter -> entry -> unit
      val parse : filename:string -> tablename:string -> strtab:string ->
	Sh.entry list -> entry list
    end
    module Decode : sig
      val start : filename:string -> secname:string -> Sh.entry list -> unit
    end
end);;
