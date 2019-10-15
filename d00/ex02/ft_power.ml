let ft_power x n =
  let rec ft_power_r x n r =
    if n == 0 then r
    else ft_power_r x (n - 1) (r * x) in
  ft_power_r x n 1

let () =
  let test_ft_power (x, n) output =
    let r = ft_power x n in
    if r == output
    then Printf.printf "SUCCESS: ft_power %d %d = %d\n" x n r
    else Printf.printf "FAILURE: ft_power %d %d = %d <> %d\n" x n r output
    in
  test_ft_power (2,4) 16;
  test_ft_power (3,0) 1;
  test_ft_power (0,5) 0;
