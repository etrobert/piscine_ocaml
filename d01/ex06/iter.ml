let rec iter p x n =
  if n < 0 then -1
  else if n = 0 then x
  else iter p (p x) (n - 1)

let () =
  let test_square ((x, n), output) =
    let r = iter (fun x -> x * x) x n in
    if r = output
    then Printf.printf "SUCCESS: iter (fun x -> x * x) %d %d = %d\n" x n r
    else
      Printf.printf
        "FAILURE: iter (fun x -> x * x) %d %d = %d <> %d\n" x n r output
  in
  let tests = [
    (2, 4), 65536;
    (2, (-1)), (-1);
    (1, 5), 1;
  ] in
  List.iter test_square tests;

  let test_double ((x, n), output) =
    let r = iter (fun x -> x * 2) x n in
    if r = output
    then Printf.printf "SUCCESS: iter (fun x -> x * 2) %d %d = %d\n" x n r
    else
      Printf.printf
        "FAILURE: iter (fun x -> x * 2) %d %d = %d <> %d\n" x n r output
  in
  let tests_double = [
    (2, 4), 32;
    (2, (-1)), (-1);
    (1, 5), 32;
  ] in
  List.iter test_double tests_double
