let rec repeat_x n =
  if n < 0 then "Error"
  else if n = 0 then ""
  else repeat_x (n - 1) ^ "x"

let () =
  let test (input, output) =
    let r = repeat_x input in
    if r = output
    then Printf.printf "SUCCESS: repeat_x %d = \"%s\"\n" input output
    else
      Printf.printf "FAILURE: repeat_x %d = \"%s\" <> \"%s\"\n" input r output
    in
  let tests = [
    -1, "Error";
    0, "";
    1, "x";
    3, "xxx";
    5, "xxxxx"
  ] in
  List.iter test tests
