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

let print_nucleotide (p,d,n) =
  print_endline p;
  print_endline d;
  print_endline
    (match n with
     | A -> "A"
     | T -> "T"
     | C -> "C"
     | G -> "G"
     | None -> "None")

let () =
  print_nucleotide (generate_nucleotide 'A');
  print_nucleotide (generate_nucleotide 'T');
  print_nucleotide (generate_nucleotide 'C');
  print_nucleotide (generate_nucleotide 'G');
  print_nucleotide (generate_nucleotide 'B')
