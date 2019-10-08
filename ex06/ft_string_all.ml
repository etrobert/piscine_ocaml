(*
  This could be optimized.
  Currently the function will continue checking
  even though a character failed the test.
*)

let ft_string_all p s =
  (* If the predicates says false the acc is set to true *)
  not (Seq.fold_left (fun acc ch -> acc || not (p ch)) false (String.to_seq s))

let () =
  let is_digit c = c >= '0' && c <= '9' in
  Printf.printf "%B\n" (ft_string_all is_digit "0123456789");
  Printf.printf "%B\n" (ft_string_all is_digit "012345A789");
