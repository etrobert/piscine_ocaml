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
  ft_print_rev "Hello World !";
  ft_print_rev ""
