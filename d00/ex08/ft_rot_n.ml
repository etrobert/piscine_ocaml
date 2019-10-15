let ft_rot_n n s =
  (*
    The function rotate takes a char,
    transforms it to a 'simple char', a mapping from min..max to 0..x
    applies the transformation ( + n mod (range + 1) )
    then transforms it back to a normal char
  *)
  let rotate min max c =
    let range = int_of_char max - int_of_char min in
    let to_simple c = int_of_char c - int_of_char min in
    let from_simple s = char_of_int (s + int_of_char min) in
    from_simple ((to_simple c + n) mod (range + 1))
    in
  let f c =
    if c >= 'a' && c <= 'z' then rotate 'a' 'z' c
    else if c >= 'A' && c <= 'Z' then rotate 'A' 'Z' c
    else c
    in
  String.map f s

let () =
  let test_ft_rot_n (n, s) output =
    let r = ft_rot_n n s in
    if r = output
    then Printf.printf "SUCCESS: ft_rot_n %d \"%s\" = \"%s\"\n" n s output
    else
      Printf.printf "FAILURE: ft_rot_n %d \"%s\" = \"%s\" <> \"%s\"\n" n s r output
    in
  test_ft_rot_n (1, "abcdefghijklmnopqrstuvwxyz") "bcdefghijklmnopqrstuvwxyza";
  test_ft_rot_n (13,  "abcdefghijklmnopqrstuvwxyz") "nopqrstuvwxyzabcdefghijklm";
  test_ft_rot_n (42,  "0123456789") "0123456789";
  test_ft_rot_n (2,  "OI2EAS67B9") "QK2GCU67D9";
  test_ft_rot_n (0,  "Damned !") "Damned !";
  test_ft_rot_n (42,  "") "";
  test_ft_rot_n (1,  "NBzlk qnbjr !") "OCaml rocks !"
