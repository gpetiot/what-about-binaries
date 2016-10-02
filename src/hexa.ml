
let char_hexa = function
  | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4'
  | 5 -> '5' | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9'
  | 10 -> 'a' | 11 -> 'b' | 12 -> 'c' | 13 -> 'd' | 14 -> 'e' | 15 -> 'f'
  | x -> failwith (Format.sprintf "Hexa.char_hexa %i" x)
;;

let pretty fmt i =
  let rec aux n p inc = function
    | [] -> n, p
    | pow2 :: t ->
       aux (n mod pow2) (if n / pow2 = 1 then p + inc else p) (inc / 2) t
  in
  let j, p1 = aux i 0 8 [128; 64; 32; 16] in
  let _, p2 = aux j 0 8 [8; 4; 2; 1] in
  Format.fprintf fmt "%c%c" (char_hexa p1) (char_hexa p2)
;;
