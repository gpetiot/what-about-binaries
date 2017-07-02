
type eclass;;

val of_int: int -> eclass;;
val pretty: Format.formatter -> eclass -> unit;;
  
module type Addr =
sig
  val eclass : eclass
  val size : int
  val header_size : int
  val modes : Capstone.mode list
  (* program header field sizes *)
  val type_size : int
  val offset_size : int
  val vaddr_size : int
  val paddr_size : int
  val filesz_size : int
  val memsz_size : int
  val flags_size : int
  val align_size : int
  (* program header field offsets *)
  val type_offset : int
  val offset_offset : int
  val vaddr_offset : int
  val paddr_offset : int
  val filesz_offset : int
  val memsz_offset : int
  val flags_offset : int
  val align_offset : int
  (* symbol field sizes *)
  val name_size : int
  val value_size : int
  val size_size : int
  val info_size : int
  val other_size : int
  val shndex_size : int
  (* symbol field offsets *)
  val name_offset : int
  val info_offset : int
  val other_offset : int
  val shndex_offset : int
  val value_offset : int
  val size_offset : int
end;;

val addr: eclass -> (module Addr);;
