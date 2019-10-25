class virtual reaction
    ((l1:Molecule.molecule list), (l2:Molecule.molecule list)) =
  object (self)
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool

    method to_string : string =
      let print_coefs l =
        List.fold_left
          (fun acc (m, k) -> acc ^ string_of_int k ^ m#formula ^ " ")
          ""
          l
      in
      print_coefs self#get_start ^ "-> " ^ print_coefs self#get_result
  end
