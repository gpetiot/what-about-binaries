
module Visibility = struct
  type t = Default | Hidden
  let of_int = function
    | 0 -> Default
    | 2 -> Hidden
    | x -> failwith (Printf.sprintf "Symbol.Visibility.of_int %i" x)
  let to_string = function
    | Default -> "DEFAULT"
    | Hidden -> "HIDDEN"
  let pretty fmt x = Format.fprintf fmt "%-8s" (to_string x);;
end

module Ndx = struct
  type t = Abs | Und | Int of int
  let of_int = function
    | 0 -> Und
    | 65521 -> Abs
    | x -> Int x
  let to_string = function
    | Abs -> "ABS"
    | Und -> "UND"
    | Int x -> string_of_int x
  let pretty fmt x = Format.fprintf fmt "%3s" (to_string x);;
end

module Binding = struct
  type t = Local | Global | Weak
  let of_int = function
    | 0 -> Local
    | 1 -> Global
    | 2 -> Weak
    | x -> failwith (Printf.sprintf "Symbol.Binding.of_int %i" x)
  ;;
  let to_string = function
    | Local -> "LOCAL"
    | Global -> "GLOBAL"
    | Weak -> "WEAK"
  ;;
  let pretty fmt x = Format.fprintf fmt "%-6s" (to_string x);;
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
    | Notype -> "NOTYPE"
    | Object -> "OBJECT"
    | Func -> "FUNC"
    | Section -> "SECTION"
    | File -> "FILE"
  ;;
  let pretty fmt x = Format.fprintf fmt "%-7s" (to_string x);;
end;;
