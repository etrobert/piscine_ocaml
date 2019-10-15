let rec ackermann m n =
  match m,n with
  | m, n when m < 0 || n < 0 -> -1
  | 0, n -> n + 1
  | m, 0 -> ackermann (m - 1) 1
  | m, n -> ackermann (m - 1) (ackermann m (n - 1))

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
