let ft_sum e lb ub =
  let rec loop e lb ub acc =
    if lb > ub then acc
    else loop e (lb + 1) ub (acc +. (e lb))
  in
  if lb > ub then nan else loop e lb ub 0.0

let () =
  let test_square ((lb, ub), output) =
    let r = ft_sum (fun i -> float_of_int (i * i)) lb ub in
    Printf.printf (if r = output then "SUCCESS" else "FAILURE");
    Printf.printf ": ft_sum (fun i -> float_of_int (i * i) %d %d = %f" lb ub r;
    if r <> output then Printf.printf " <> %f\n" output else print_char '\n'
  in
  let tests_square = [
    (1, 10), 385.0;
    (1, 2), 5.0;
  ] in
  List.iter test_square tests_square
