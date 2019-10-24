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
