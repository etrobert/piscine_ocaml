(* TODO not done *)

let ft_rot_n n s =
  (*
    The function f takes a char,
    transforms it to a 'simple char', a mapping from 'a'..'z' to 0..25
    applies the transformation ( + n % az_range )
    then transforms it back to a normal char
  *)
  let f c =
    let az_range = Char.code 'z' - Char.code 'a' in
    let to_simple c = Char.code c - Char.code 'a' in
    let from_simple s = Char.chr (s + Char.code 'a') in
    from_simple ((to_simple c + n) mod az_range) in
  String.map f s

let test_rot_n n s =
  Printf.printf "%s (%d) ->\n%s\n" s n (ft_rot_n n s)

let () =
  test_rot_n 1 "abcdefghijklmnopqrstuvwxyz";
  test_rot_n 13 "abcdefghijklmnopqrstuvwxyz";
  test_rot_n 42 "0123456789"
