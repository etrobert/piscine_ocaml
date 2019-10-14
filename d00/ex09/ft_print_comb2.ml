let ft_print_comb2 () =
  let rec ft_print_comb2_r comb =
    let next_comb = function
      | (a,99) -> a + 1, a + 2
      | (a,b) -> a, b + 1
      in
    let print_comb (a,b) =
      let print_number k = if k < 10 then print_char '0'; print_int k in
      print_number a;
      print_char ' ';
      print_number b
      in
    print_comb comb;
    if comb <> (98,99) then (
      print_char ',';
      print_char ' ';
      ft_print_comb2_r (next_comb comb)
    )
    in
    ft_print_comb2_r (0,1);
    print_char '\n'

let () =
  ft_print_comb2 ()
