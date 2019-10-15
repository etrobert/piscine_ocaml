let encode l =
  let rec loop l f item count =
    match l with
    | [] -> f [(count, item)]
    | x::tail when x = item -> loop tail f item (count + 1)
    | x::tail               -> loop tail (fun l -> f ((count, item)::l)) x 1
  in
  match l with
  | [] -> []
  | x::tail -> loop tail (fun l -> l) x 1

let () =
  let print_list printer l =
    print_string "[";
    let rec loop printer l =
      match l with
      | [] -> print_string "]"
      | x::t -> printer x; print_string "; "; loop printer t
    in
    loop printer l
  in
  let test printer (input, output) =
    let r = encode input in
    Printf.printf (if r = output then "SUCCESS" else "FAILURE");
    Printf.printf ": encode ";
    print_list printer input;
    Printf.printf " => ";
    let print_value printer (a, b) =
      print_string "(";
      print_int a;
      print_string ", ";
      printer b;
      print_string ")";
    in
    let print_encoded = print_list (print_value printer) in
    print_encoded r;
    if r <> output
    then (print_string " <> "; print_encoded output);
    print_char '\n'
  in
  let tests_int = [
    [], [];
    [1], [(1, 1)];
    [1;1;1;2;3;4], [(3, 1); (1, 2); (1, 3); (1, 4)];
    [5;2;1;2;3;4], [(1, 5); (1, 2); (1, 1); (1, 2); (1, 3); (1, 4)];
  ] in
  List.iter (test print_int) tests_int;
  let tests_string = [
    [], [];
    ["BONJOUR"; "BONJOUR"], [(2, "BONJOUR")];
    ["a"; "b"], [(1, "a"); (1, "b")];
  ] in
  List.iter (test print_string) tests_string
