let ft_print_comb () =
  let rec ft_print_comb_r comb =
    let next_comb (a, b, c) =
      if b = 8 && c = 9 then a + 1, a + 2, a + 3
      else if c = 9 then a, b + 1, b + 2
      else a, b, c + 1
      in
    let print_comb (a,b,c) =
      print_int a;
      print_int b;
      print_int c
      in
    print_comb comb;
    if comb <> (7,8,9) then (
      print_string ", ";
      ft_print_comb_r (next_comb comb)
    )
    in
    ft_print_comb_r (0,1,2);
    print_string "\n"

let () =
  ft_print_comb ()
