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

let id x = x

let rec fold_right f l acc =
  let rec loop f2 l =
    match l with
    | [] -> f2 acc
    | hd::tl -> loop (fun acc -> f2 (f hd acc)) tl
  in
  loop id l

let sequence n =
  let rec loop f n =
    let flatten l =
      fold_right (fun (a,b) acc -> a::b::acc) l []
    in
    if n = 0 then f [1]
    else loop (fun l -> f (flatten (encode l))) (n - 1)
  in
  let string_of_int_list l =
    fold_right (fun x acc -> string_of_int x ^ acc) l ""
  in
  if n < 0 then "" else string_of_int_list (loop id n)

let print_list printer l =
  print_string "[";
  let rec loop printer l =
    match l with
    | [] -> print_string "]"
    | x::t -> printer x; print_string "; "; loop printer t
  in
  loop printer l

let rec iter f l =
  match l with
  | [] -> ()
  | hd::tl -> f hd; iter f tl

let () =
  iter (fun x -> print_string (sequence x); print_char '\n') [(-1);0;1;2;3;4;5;6;7]
