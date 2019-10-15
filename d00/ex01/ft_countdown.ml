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
  let test_ft_countdown input output =
    Printf.printf "ft_countdown %d should give %s :\n" input output;
    ft_countdown input
    in
  test_ft_countdown 3 "3 2 1 0";
  test_ft_countdown 0 "0";
  test_ft_countdown (-1) "0"
