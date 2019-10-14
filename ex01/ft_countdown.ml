let rec ft_countdown n =
  let print_value n =
    print_int n;
    print_char '\n'
    in
  if n <= 0 then print_value 0
  else
    begin
      print_value n;
      ft_countdown (n - 1)
    end

let () =
  ft_countdown 3;
  ft_countdown 0;
  ft_countdown (-1)
