(* Standard Library Functions *)

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | hd::tl -> fold_left f (f acc hd) tl

(* Exercise 02 *)

let caesar n = String.map (fun c -> char_of_int ((int_of_char c) + n mod 256))
let rot42 = caesar 42
let xor n = String.map (fun c -> char_of_int (int_of_char c lxor n))

let ft_crypt = fold_left (fun acc f -> f acc)

(* Tests *)

let mult n = String.map (fun c -> char_of_int ((int_of_char c) * n mod 256))

let () =
  let s = "B1234567890!@#$%^&*()asdfghjkl=/*-+" in
  let n = 3 in
  let cs = (caesar n s) in
  Printf.printf "caesar %d \"%s\" = \"%s\"\n" n s cs;

  let n = 3 in
  let xs = (xor n s) in
  Printf.printf "xor %d \"%s\" = \"%s\"\n" n s xs;
  Printf.printf "xor %d \"%s\" = \"%s\"\"\n" n xs (xor n xs);

  let alph = "abcdefghijklmnopqrstuvwxyz" in
  let crypts = (ft_crypt alph [(mult 2); (caesar 1)] ) in
  let print_int_of_char c = print_int (int_of_char c); print_char ' ' in
  String.iter print_int_of_char alph;
  print_char '\n';
  String.iter print_int_of_char crypts;

  print_endline (ft_crypt alph [caesar (-42); rot42; xor 42])
