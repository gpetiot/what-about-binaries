
open Elf_types

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
  | LittleEndian -> (module Little : Elf_types.Endianness)
  | BigEndian -> (module Big : Elf_types.Endianness)
;;
