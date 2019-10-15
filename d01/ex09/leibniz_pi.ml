let leibniz_pi delta =
  let rec loop delta it value =
    let f i =
      (if i mod 2 == 0 then 4.0 else (-4.0)) /.
      (2.0 *. (float_of_int i) +. 1.0) in
    let pi = 4.0 *. atan 1.0 in
    let fabs n = if n >= 0.0 then n else (-.n) in
    if fabs (value -. pi) <= delta
    then it
    else loop delta (it + 1) (value +. (f it))
  in
  if delta <= 0.0 then -1 else loop delta 0 0.0

let () =
  let test (input, output) =
    let r = leibniz_pi input in
    Printf.printf (if r = output then "SUCCESS" else "FAILURE");
    Printf.printf ": leibniz_pi %f = %d" input r;
    if r <> output then Printf.printf " <> %d\n" output else print_char '\n'
  in
  let tests = [
    (-1.0), (-1);
    5.0, 0;
    1.0, 1;
    0.5, 2;
    0.4, 3;
  ] in
  List.iter test tests

