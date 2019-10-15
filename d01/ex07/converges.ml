let rec converges f x n =
  let f_x = f x in
  if n < 0 then false
  else if x = f_x then true
  else if n = 0 then false
  else converges f f_x (n - 1)

let () =
  let test_times_two ((x, n), output) =
    let r = converges (( * ) 2) x n in
    Printf.printf (if r = output then "SUCCESS" else "FAILURE");
    Printf.printf ": converges %d %d = %B" x n r;
    if r <> output then Printf.printf " <> %B\n" output else print_char '\n'
  in
  let tests_times_two = [
    (2, 5), false;
  ] in
  List.iter test_times_two tests_times_two;

  let test_halve ((x, n), output) =
    let r = converges (fun x -> x / 2) x n in
    Printf.printf (if r = output then "SUCCESS" else "FAILURE");
    Printf.printf ": converges %d %d = %B" x n r;
    if r <> output then Printf.printf " <> %B\n" output else print_char '\n'
  in
  let tests_halve = [
    (2, 3), true;
    (2, 2), true;
    (2, 1), false;
  ] in
  List.iter test_halve tests_halve
