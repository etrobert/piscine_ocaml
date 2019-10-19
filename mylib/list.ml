let list_to_string to_string l =
  List.fold_left (fun acc x -> acc ^ to_string x ^ "; ") "[" l ^ "]"
