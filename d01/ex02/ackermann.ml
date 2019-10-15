let rec ackermann m n =
  if m < 0 || n < 0 then -1
  else if m = 0 then n + 1
  else if n = 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

let () =
  let test ((m, n), output) =
    let r = ackermann m n in
    if r = output
    then Printf.printf "SUCCESS: ackermann %d %d = %d\n" m n r
    else
      Printf.printf "FAILURE: ackermann %d %d = %d <> %d\n" m n r output
    in
  let tests = [
    (-1, 7), (-1);
    (0, 0), 1;
    (2, 3), 9;
    (4, 1), 65533
  ] in
  List.iter test tests
