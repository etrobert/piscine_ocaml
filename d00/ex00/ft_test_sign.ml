let ft_test_sign n =
  if n < 0 then
    print_endline "negative"
  else
    print_endline "positive"

let () =
  let test_ft_test_sign input output =
    Printf.printf "ft_test_sign(%d) should give \"%s\" : " input output;
    ft_test_sign input
    in

  test_ft_test_sign 42 "positive";
  test_ft_test_sign 0 "positive";
  test_ft_test_sign (-42) "negative";
