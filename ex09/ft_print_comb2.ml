let ft_print_comb2 =
  let rec ft_print_comb2_r comb =
    let next_comb = function
      | (a,99) -> a + 1, a + 2
      | (a,b) -> a, b + 1
      in
    let print_comb (a,b) =
      Printf.printf "%02d %02d" a b
      in
    print_comb comb;
    if comb <> (98,99) then (
      print_string ", ";
      ft_print_comb2_r (next_comb comb)
    )
    in
    fun () -> ft_print_comb2_r (0,1);
    print_char '\n'

let () =
  ft_print_comb2 ()
