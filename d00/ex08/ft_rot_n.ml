let ft_rot_n n s =
  (*
    The function rot_chr takes a char,
    transforms it to a 'simple char', a mapping from min..max to 0..max-min
    applies the transformation ( + n % range_size )
    then transforms it back to a normal char
  *)
  let f c =
    let rot_chr min max c =
      let range_size = int_of_char max - int_of_char min + 1 in
      let to_simple c = int_of_char c - int_of_char min in
      let from_simple s = char_of_int (s + int_of_char min) in
      from_simple ((to_simple c + n) mod range_size) in
    let in_range min max x = x >= min && x <= max in
    match c with
      | c when in_range 'a' 'z' c -> rot_chr 'a' 'z' c
      | c when in_range 'A' 'Z' c -> rot_chr 'A' 'Z' c
      | c -> c in
  String.map f s


let () =
  let test_rot_n n s =
    Printf.printf "%s (%d) ->\n%s\n" s n (ft_rot_n n s)
    in
  test_rot_n 1 "abcdefghijklmnopqrstuvwxyz";
  test_rot_n 13 "abcdefghijklmnopqrstuvwxyz";
  test_rot_n 42 "0123456789";
  test_rot_n 2 "OI2EAS67B9";
  test_rot_n 0 "Damned !";
  test_rot_n 42 "";
  test_rot_n 1 "NBzlk qnbjr !"
