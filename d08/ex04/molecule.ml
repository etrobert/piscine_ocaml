let encode equal l =
  let rec loop l item count =
    match l with
    | [] -> [(count, item)]
    | x::tail when equal x item -> loop tail item (count + 1)
    | x::tail               -> (count, item)::(loop tail x 1)
  in
  match l with
  | [] -> []
  | x::tail -> loop tail x 1

let generate_hill_fomula l =
  let string_of_count_atom =
    function
    | (0, _) -> ""
    | (1, atom) -> atom#symbol
    | (count, atom) -> atom#symbol ^ string_of_int count in
  let string_of_atom_list = function a_list ->
    let sorted_atoms = List.sort (fun a1 a2 -> compare a1#name a2#name) a_list in
    let encoded_atoms = encode (fun a b -> a#equals b) sorted_atoms in
    String.concat "" (List.map string_of_count_atom encoded_atoms)
  in
  let carbon = new Atom.carbon in
  let (carbons, w_carbons) = List.partition carbon#equals l in
  let carbon_count = List.length carbons in
  if carbon_count = 0
  then string_of_atom_list w_carbons
  else
    let hydrogen = new Atom.hydrogen in
    let (hydrogens, w_hydrogens) = List.partition hydrogen#equals w_carbons in
    string_of_count_atom (carbon_count, carbon) ^
    string_of_count_atom (List.length hydrogens, hydrogen) ^
    string_of_atom_list w_hydrogens


class virtual molecule (name, (al:Atom.atom list)) =
  object (self)
    method name : string = name
    method formula = generate_hill_fomula al
    method to_string = name ^ "(" ^ self#formula ^ ")"
    method equals = function (m:molecule) -> m#formula = self#formula
  end

class water =
  object
    inherit molecule
        ("Water",
         let h = new Atom.hydrogen in
         let o = new Atom.oxygen in
         [h; o; h])
  end

class carbon_dioxyde =
  object
    inherit molecule
        ("Carbon diocyde",
         let o = new Atom.oxygen in
         [new Atom.carbon; o; o])
  end

class trinitrotoluene =
  object
    inherit molecule
        ("Trinitoluene",
         List.init 3 (function _ -> new Atom.nitrogen) @
         List.init 5 (function _ -> new Atom.hydrogen) @
         List.init 6 (function _ -> new Atom.oxygen) @
         List.init 7 (function _ -> new Atom.carbon)
        )
  end

class oxygen =
  object
    inherit molecule
        ("Oxygen",
         let o = new Atom.oxygen in
         [o; o])
  end

class methane =
  object
    inherit molecule
        ("Methane",
         new Atom.carbon::List.init 4 (function _ -> new Atom.hydrogen) )
  end
