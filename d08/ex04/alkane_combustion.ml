let remove_duplicates (l:Alkane.alkane list) : (Alkane.alkane list) =
  List.sort_uniq
    (fun x y -> if x#equals (y :> Molecule.molecule) then 0 else 1)
    l

class alkane_combustion (l:Alkane.alkane list) =
  object (self)
    inherit Reaction.reaction
        (new Molecule.oxygen::(l :> Molecule.molecule list),
         new Molecule.carbon_dioxyde::new Molecule.water::[])
    val start_result :
      ( ((Molecule.molecule * int) list) *
        ((Molecule.molecule * int) list)) option = None
    method get_start = match start_result with
      | None -> failwith "Reaction is not balanced"
      | Some (start, _) -> start
    method get_result = match start_result with
      | None -> failwith "Reaction is not balanced"
      | Some (_, result) -> result
    method balance =
      let l : Alkane.alkane list = remove_duplicates l in
      let rec loop (alkanes, ox, cd, wa) =
        function
        | [] -> (alkanes, ox, cd, wa)
        | hd::tail ->
          let c = hd#carbons in
          if c mod 2 <> 0
          then loop ((hd, 1)::alkanes,
                     ox + ((3 * c) + 1) / 2,
                     cd + c,
                     wa + c + 1) tail
          else loop ((hd, 2)::alkanes,
                     ox + (3 * c) + 1,
                     cd + 2 * c,
                     cd + 2 * c + 2) tail in
      let (alkanes, ox, cd, wa) = loop ([], 0, 0, 0) l in
      ({< start_result = Some (
           (new Molecule.oxygen, ox)::(alkanes :> (Molecule.molecule * int) list),
           (new Molecule.carbon_dioxyde, cd)::(new Molecule.water, wa)::[]
         ) >} :> Reaction.reaction)

    method is_balanced = match start_result with None -> false | _ -> true
  end
