let id x = x

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

let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec init n f =
  if n < 0 then [] else (f (n - 1))::(init (n - 1) f)

let rev l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | hd::tl -> loop tl (hd::acc)
  in
  loop l []

(* Exercise 04 *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

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
  | U -> "U"
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
           | U -> 'U'
           | None -> ' ')) h


(* Exercise 06 *)

type rna = nucleobase list

let generate_rna (h:helix) : rna =
  map (fun (_,_,n) -> match n with
      | A -> U
      | T -> A
      | C -> G
      | G -> C
      | U -> None
      | None -> None
    ) h


(* Exercise 07 *)

let generate_bases_triplets (r:rna) =
  rev (fst (fold_left (fun (l, t) x ->
      match t with
      | None, None -> l, (x, None)
      | a, None -> l, (a, x)
      | a, b -> (a,b,x)::l, (None, None)
    ) ([], (None, None)) r))

type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list

let string_of_aminoacid = function
  | Stop -> "End of translation"
  | Ala -> "Alanine"
  | Arg -> "Arginine"
  | Asn -> "Asparagine"
  | Asp -> "Aspartique"
  | Cys -> "Cysteine"
  | Gln -> "Glutamine"
  | Glu -> "Glutamique"
  | Gly -> "Glycine"
  | His -> "Histidine"
  | Ile -> "Isoleucine"
  | Leu -> "Leucine"
  | Lys -> "Lysine"
  | Met -> "Methionine"
  | Phe -> "Phenylalanine"
  | Pro -> "Proline"
  | Ser -> "Serine"
  | Thr -> "Threonine"
  | Trp -> "Tryptophane"
  | Tyr -> "Tyrosine"
  | Val -> "Valine"

let string_of_protein (p:protein) =
  fold_left (fun acc x -> acc ^ string_of_aminoacid x ^ "\n") "" p

let decode_rna (r:rna) : protein =
  let rec loop triplets acc =
    match triplets with
    | [] -> acc
    | (U, A, A)::(U, A, G)::(U, G, A)::tl -> acc
    | (G, C, A)::(G, C, C)::(G, C, C)::(G, C, U)::tl -> loop tl (Ala::acc)
    | (A, G, A)::(A, G, G)::(C, G, A)::(C, G, C)::(C, G, G)::(C, G, U)::tl -> loop tl (Arg::acc)
    | (A, A, C)::(A, A, U)::tl -> loop tl (Asn::acc)
    | (G, A, C)::(G, A, U)::tl -> loop tl (Asp::acc)
    | (U, G, C)::(U, G, U)::tl -> loop tl (Cys::acc)
    | (C, A, A)::(C, A, G)::tl -> loop tl (Gln::acc)
    | (G, A, A)::(G, A, G)::tl -> loop tl (Glu::acc)
    | (C, G, A)::(G, G, C)::(G, G, G)::(G, G, U)::tl -> loop tl (Gly::acc)
    | (C, A, C)::(C, A, U)::tl -> loop tl (His::acc)
    | (A, U, A)::(A, U, C)::(A, U, U)::tl -> loop tl (Ile::acc)
    | (C, U, A)::(C, U, C)::(C, U, G)::(C, U, U)::(U, U, A)::(U, U, G)::tl -> loop tl (Leu::acc)
    | (A, A, A)::(A, A, G)::tl -> loop tl (Lys::acc)
    | (A, U, G)::tl -> loop tl (Met::acc)
    | (U, U, C)::(U, U, U)::tl -> loop tl (Phe::acc)
    | (C, C, C)::(C, C, A)::(C, C, G)::(C, C, U)::tl -> loop tl (Pro::acc)
    | (U, C, A)::(U, C, C)::(U, C, G)::(U, C, U)::(A, G, U)::(A, G, C)::tl -> loop tl (Ser::acc)
    | (A, C, A)::(A, C, C)::(A, C, G)::(A, C, U)::tl -> loop tl (Thr::acc)
    | (U, G, G)::tl -> loop tl (Trp::acc)
    | (U, A, C)::(U, A, U)::tl -> loop tl (Trp::acc)
    | (G, U, A)::(G, U, C)::(G, U, G)::(G, U, U)::tl -> loop tl (Val::acc)
    | hd::tl -> loop tl acc
  in
  rev (loop (generate_bases_triplets r) [])

(* Exercise 08 *)

let rna_to_string (h:rna) =
  fold_right (fun n acc -> nucleobase_to_string n ^ acc) h ""

let life name =
  print_endline "Initializing process...";
  print_endline "Heating the machine...";
  print_endline "Generating helix...";
  let h = generate_helix 999 in
  print_endline (helix_to_string h);
  print_endline "Generating RNA...";
  let r = generate_rna h in
  print_endline (rna_to_string r);
  print_endline "Generating protein by decoding RNA...";
  let p = decode_rna r in
  print_string (string_of_protein p);
  print_endline "It is alive !!!";
  print_string name;
  print_endline ": \"Beware; for I am fearless, and therefore powerful.\""

(* Tests *)

let () = life "Edgar"
