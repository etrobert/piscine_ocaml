let fibonacci n =
  let rec fibonacci_r a b n =
    if n = 0 then b
    else fibonacci_r (a + b) a (n - 1)
    in
  if n < 0 then (-1)
  else fibonacci_r 1 0 n

let () =
  let test (input, output) =
    let r = fibonacci input in
    if r = output
    then Printf.printf "SUCCESS: fibonacci %d = %d\n" input r
    else
      Printf.printf "FAILURE: fibonacci %d = %d <> %d\n" input r output
    in
  let tests = [
    (-42), (-1);
    1, 1;
    3, 2;
    6, 8
  ] in
  List.iter test tests
