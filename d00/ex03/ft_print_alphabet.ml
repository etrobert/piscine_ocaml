let ft_print_alphabet () =
  let rec ft_print_alphabet_r c =
    if c == 'z' then (
      print_char 'z';
      print_char '\n'
    ) else (
      print_char c;
      ft_print_alphabet_r (char_of_int (int_of_char c + 1))
    ) in
    ft_print_alphabet_r 'a'

let () =
  ft_print_alphabet ()
