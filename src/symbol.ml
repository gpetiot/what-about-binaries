
module Binding = struct
  type t = Local | Global | Weak
  let of_int = function
    | 0 -> Local
    | 1 -> Global
    | 2 -> Weak
    | x -> failwith (Printf.sprintf "Symbol.Binding.of_int %i" x)
  ;;
  let to_string = function
    | Local -> "local"
    | Global -> "global"
    | Weak -> "weak"
  ;;
  let pretty fmt x = Format.fprintf fmt "%s" (to_string x);;
end;;

module Type = struct
  type t = Notype | Object | Func | Section | File
  let of_int = function
    | 0 -> Notype
    | 1 -> Object
    | 2 -> Func
    | 3 -> Section
    | 4 -> File
    | x -> failwith (Printf.sprintf "Symbol.Type.of_int %i" x)
  ;;
  let to_string = function
    | Notype -> "notype"
    | Object -> "object"
    | Func -> "func"
    | Section -> "section"
    | File -> "file"
  ;;
  let pretty fmt x = Format.fprintf fmt "%s" (to_string x);;
end;;
