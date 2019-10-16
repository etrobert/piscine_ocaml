let id x = x

let rec fold_right f l acc =
  let rec loop f2 l =
    match l with
    | [] -> f2 acc
    | hd::tl -> loop (fun acc -> f2 (f hd acc)) tl
  in
  loop id l

let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec init n f =
  if n < 0 then [] else (f (n - 1))::(init (n - 1) f)

(* Exercise 04 *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucbase : nucleotide =
  "phosphate",
  "deoxyribose",
  match nucbase with
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> None

(* Exercise 05 *)

type helix = nucleotide list

let generate_helix n : helix =
  init n (fun _ -> generate_nucleotide
             (match Random.int 4 with
              | 0 -> 'A'
              | 1 -> 'T'
              | 2 -> 'C'
              | 3 -> 'G'
              | _ -> ' '))

let nucleobase_to_string = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | None -> "None"

let helix_to_string (h:helix) =
  fold_right (fun (p,d,n) acc -> nucleobase_to_string n ^ acc) h ""

let complimentary_helix (h:helix) : helix =
  map (fun (p,d,n) -> generate_nucleotide
          (match n with
           | A -> 'T'
           | T -> 'A'
           | C -> 'G'
           | G -> 'C'
           | None -> ' ')) h

(* Tests *)

let print_list printer l =
  print_string "[";
  let rec loop printer l =
    match l with
    | [] -> print_string "]"
    | x::t -> printer x; print_string "; "; loop printer t
  in
  loop printer l

let () =
  Random.self_init ();
  let h = generate_helix 6 in
  print_endline (helix_to_string h);
  print_endline (helix_to_string (complimentary_helix h));
