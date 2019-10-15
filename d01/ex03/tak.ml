let rec tak x y z =
  if y < x then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  else z

let () =
  let test ((x, y, z), output) =
    let r = tak x y z in
    if r = output
    then Printf.printf "SUCCESS: ackermann %d %d %d = %d\n" x y z r
    else
      Printf.printf "FAILURE: ackermann %d %d %d = %d <> %d\n" x y z r output
    in
  let tests = [
    (1, 2, 3), 3;
    (5, 23, 7), 7;
    (1, 1, 1), 1;
    (0, 42, 0), 0;
    (23498, 98734, 98776), 98776;
  ] in
  List.iter test tests
