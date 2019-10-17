(* Standard Library Functions *)

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | hd::tl -> fold_left f (f acc hd) tl

(* Exercise 02 *)

let uncaesar n = String.map (fun c -> char_of_int ((int_of_char c) - n mod 256))
let unrot42 = uncaesar 42
let xor n = String.map (fun c -> char_of_int (int_of_char c lxor n))

let ft_uncrypt = fold_left (fun acc f -> f acc)

let () =
  let cs = "E456789:;<3$C&'(a)-+,dvgijkmno@2-0." in
  let n = 3 in
  Printf.printf "uncaesar %d \"%s\" = \"%s\"\"\n" n cs (uncaesar n cs);

  let crs = "KHINOLMBC@AFGDEZ[XY^_\\]RSP" in
  print_endline (ft_uncrypt crs [xor 42; unrot42; uncaesar (-42)])
