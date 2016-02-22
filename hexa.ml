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

let char_hexa = function
  | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4'
  | 5 -> '5' | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9'
  | 10 -> 'A' | 11 -> 'B' | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
  | _ -> failwith "char_hexa"
;;

let to_string i =
  let rec aux n p inc = function
    | [] -> n, p
    | pow2 :: t ->
       aux (n mod pow2) (if n / pow2 = 1 then p + inc else p) (inc / 2) t
  in
  let j, p1 = aux i 0 8 [128; 64; 32; 16] in
  let _, p2 = aux j 0 8 [8; 4; 2; 1] in
  Printf.sprintf "%c%c" (char_hexa p1) (char_hexa p2)
;;
