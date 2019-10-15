let ft_print_rev s =
  let rec ft_print_rev_r s i =
    if i >= 0 then (
      print_char (String.get s i);
      ft_print_rev_r s (i - 1)
    )
    in
  ft_print_rev_r s (String.length s - 1);
  print_char '\n'

let () =
  let test_ft_print_rev input output =
    Printf.printf "ft_print_rev \"%s\" should be \"%s\": " input output;
    ft_print_rev input
    in
  test_ft_print_rev "Hello World !" "! dlroW olleH";
  test_ft_print_rev "madam" "madam";
  test_ft_print_rev "" ""
