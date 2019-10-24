let list_to_string to_string l =
  List.fold_left (fun acc x -> acc ^ to_string x ^ "; ") "[" l ^ "]"

let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rev_map f l =
  let rec loop f l acc =
    match l with
    | [] -> acc
    | hd::tl -> loop f tl ((f hd)::acc)
  in
  loop f l []

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> rev_append tl (hd::l2)

let rev l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | hd::tl -> loop tl (hd::acc)
  in
  loop l []

let id x = x

let append l1 l2 =
  let rec loop f l1 l2 =
    match l1 with
    | [] -> f l2
    | hd::tl -> loop (fun l -> f (hd::l)) tl l2
  in
  loop id l1 l2

let print_list printer l =
  print_string "[";
  let rec loop printer l =
    match l with
    | [] -> print_string "]"
    | x::t -> printer x; print_string "; "; loop printer t
  in
  loop printer l

let rec init n f =
  if n < 0 then [] else (f (n - 1))::(init (n - 1) f)

let rec iter f l =
  match l with
  | [] -> ()
  | hd::tl -> f hd; iter f tl

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | hd::tl -> fold_left f (f acc hd) tl

let rec fold_right f l acc =
  let rec loop f2 l =
    match l with
    | [] -> f2 acc
    | hd::tl -> loop (fun acc -> f2 (f hd acc)) tl
  in
  loop id l

let test_fold_right () =
  let test printer printer_out ((f, l, acc), output) =
    let r = fold_right f l acc in
    let success = output = r in
    Printf.printf (if success then "SUCCESS" else "FAILURE");
    Printf.printf ": fold_right ";
    print_list printer l;
    print_string " ";
    printer_out acc;
    Printf.printf " => ";
    printer_out r;
    if not success
    then (print_string " <> "; printer_out output);
    print_char '\n'
  in
  let tests_int = [
    ((+), [], 0), 0;
    ((+), [], 9), 9;
    ((+), [1;2;3;4], 0), 10;
    (( * ), [1;2;3;4], 1), 24;
  ] in
  List.iter (test print_int print_int) tests_int;
  let tests_int_list = [
    (List.cons, [1;2;3;4], []), [1; 2; 3; 4];
    (List.cons, [1;2;3;4], [5;6;7;8]), [1; 2; 3; 4; 5; 6; 7; 8];
    (List.cons, [], []), [];
  ]
  in
  List.iter (test print_int (print_list print_int)) tests_int_list

(*let tests_string = [
  ([], []), [];
  (["BONJOUR"], ["BONJOUR"]), ["BONJOUR"; "BONJOUR"];
  (["a"; "b"], ["a"]), ["a"; "b"; "a"];
  ] in
  List.iter (test print_string) tests_string*)

let () = test_fold_right ()

let test_append () =
  let test printer ((l1, l2), output) =
    let r = append l1 l2 in
    let success = output = r in
    Printf.printf (if success then "SUCCESS" else "FAILURE");
    Printf.printf ": append ";
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
    ([1], []), [1];
    ([], [1]), [1];
    ([1], [1]), [1; 1];
    ([1;2;3;4], [2;3]), [1;2;3;4;2;3];
  ] in
  List.iter (test print_int) tests_int;
  let tests_string = [
    ([], []), [];
    (["BONJOUR"], ["BONJOUR"]), ["BONJOUR"; "BONJOUR"];
    (["a"; "b"], ["a"]), ["a"; "b"; "a"];
  ] in
  List.iter (test print_string) tests_string

let () = test_append ()
