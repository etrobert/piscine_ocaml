let crossover l1 l2 =
  let rec exists f l =
    match l with
    | [] -> false
    | hd::tl -> if f hd then true else exists f tl
  in
  let rec fold_left f acc l =
    match l with
    | [] -> acc
    | hd::tl -> fold_left f (f acc hd) tl
  in
  fold_left
    (fun acc x -> if exists (fun y -> y = x) l2 then (x::acc) else acc) [] l1


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
  let test printer ((l1, l2), output) =
    let r = crossover l1 l2 in
    let success = output = r in
    Printf.printf (if success then "SUCCESS" else "FAILURE");
    Printf.printf ": crossover ";
    print_list printer l1;
    print_string " ";
    print_list printer l2;
    Printf.printf " => ";
    print_list printer r;
    if not success
    then (print_string " <> "; print_list printer output);
    print_char '\n'
  in
  let tests_int = [
    ([], []), [];
    ([1], []), [];
    ([], [1]), [];
    ([1], [1]), [1];
    ([1;2;3;4], [2;3]), [3;2];
  ] in
  List.iter (test print_int) tests_int;
  let tests_string = [
    ([], []), [];
    (["BONJOUR"], ["BONJOUR"]), ["BONJOUR"];
    (["a"; "b"], ["a"]), ["a"];
  ] in
  List.iter (test print_string) tests_string
