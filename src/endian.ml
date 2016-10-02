
type t = LittleEndian | BigEndian;;

let of_int = function
  | 1 -> LittleEndian
  | 2 -> BigEndian
  | x -> failwith (Format.sprintf "Endian.of_int %i" x)
;;

let pretty fmt = function
  | LittleEndian -> Format.fprintf fmt "little endian"
  | BigEndian -> Format.fprintf fmt "big endian"
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
