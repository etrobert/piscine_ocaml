let rec ft_countdown n =
  if n <= 0 then print_string "0\n"
  else
    (Printf.printf "%d\n" n; ft_countdown (n - 1))

let () =
  ft_countdown 3;
  ft_countdown 0;
  ft_countdown (-1)
