let rec repeat_string ?(str = "x") n =
  match str,n with
  | _,n when n < 0 -> "Error"
  | _,0 -> ""
  | s,n -> repeat_string ~str:str (n - 1) ^ s

let () =
  let test (input, output) =
    let r = repeat_string input in
    if r = output
    then Printf.printf "SUCCESS: repeat_string %d = \"%s\"\n" input output
    else
      Printf.printf "FAILURE: repeat_string %d = \"%s\" <> \"%s\"\n" input r output
    in
  let tests = [
    -1, "Error";
    0, "";
    1, "x";
    3, "xxx";
    5, "xxxxx"
  ] in
  List.iter test tests;

  let test_string ((str, n), output) =
    let r = repeat_string ~str:str n in
    if r = output
    then Printf.printf "SUCCESS: repeat_string ~str:\"%s\" %d = \"%s\"\n" str n output
    else
      Printf.printf "FAILURE: repeat_x ~str:\"%s\" %d = \"%s\" <> \"%s\"\n" str n r output
    in
  let tests_string = [
    ("Toto", -1), "Error";
    ("Toto", 0), "";
    ("Toto", 1), "Toto";
    ("a", 3), "aaa";
    ("what", 3), "whatwhatwhat"
  ] in
  List.iter test_string tests_string

