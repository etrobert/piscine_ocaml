let list_to_string to_string l =
  List.fold_left (fun acc x -> acc ^ to_string x ^ "; ") "[" l ^ "]"

let () =
  let h = new Atom.hydrogen in
  let c = new Atom.carbon in
  Printf.printf "%s = %s ? %B\n" h#to_string c#to_string (h#equals c);
  Printf.printf "%s = %s ? %B\n" c#to_string c#to_string (c#equals c);

  print_endline new Molecule.water#formula;
  print_endline new Molecule.carbon_dioxyde#formula;
  print_endline new Molecule.trinitrotoluene#formula;
  print_endline new Molecule.oxygen#formula;
  print_endline new Molecule.methane#formula;

  let water = new Molecule.water in
  let carbon_dioxyde = new Molecule.carbon_dioxyde in
  Printf.printf "%s = %s ? %B\n"
    carbon_dioxyde#to_string water#to_string (carbon_dioxyde#equals water);
  Printf.printf "%s = %s ? %B\n"
    carbon_dioxyde#to_string
    carbon_dioxyde#to_string
    (carbon_dioxyde#equals carbon_dioxyde);

  let methane = new Alkane.methane in
  print_endline methane#to_string;

  let ac = new Alkane_combustion.alkane_combustion [new Alkane.methane] in
  (try
     ignore ac#get_start
   with e -> print_endline (Printexc.to_string e));

  (try
     ignore ac#get_result
   with e -> print_endline (Printexc.to_string e));

  let ac = ac#balance in
  print_endline ac#to_string;
  let ac2 = new Alkane_combustion.alkane_combustion
    [new Alkane.methane; new Alkane.methane] in
  print_endline ac2#balance#to_string;
  let methane_ethane = new Alkane_combustion.alkane_combustion
    [new Alkane.methane; new Alkane.ethane] in
  print_endline methane_ethane#balance#to_string;
  let ethane = new Alkane_combustion.alkane_combustion
    [new Alkane.ethane] in
  print_endline ethane#balance#to_string



