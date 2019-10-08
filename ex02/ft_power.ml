let rec ft_power_r x n r =
  if n == 0 then r
  else ft_power_r x (n - 1) (r * x) in

let ft_power x n = ft_power_r x n 1 in

Printf.printf "%d\n" (ft_power 2 4);
Printf.printf "%d\n" (ft_power 3 0);
Printf.printf "%d\n" (ft_power 0 5);
