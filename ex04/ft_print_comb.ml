let ft_print_comb () =
  let rec ft_print_comb_r comb =
    let next_comb = function
      | (a,8,9) -> a + 1, a + 2, a + 3
      | (a,b,9) -> a, b + 1, b + 2
      | (a,b,c) -> a, b, c + 1
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
    ft_print_comb_r (0,1,2)

let () =
  ft_print_comb ()
