let ft_test_sign n =
  if n < 0 then
    Printf.printf "negative\n"
  else
    Printf.printf "positive\n"

let () =
  ft_test_sign 42;
  ft_test_sign 0;
  ft_test_sign (-42)
