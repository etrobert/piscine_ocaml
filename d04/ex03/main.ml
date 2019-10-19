let list_to_string to_string l =
  List.fold_left (fun acc x -> acc ^ to_string x ^ "; ") "[" l ^ "]"

let id x = x

let () =
  Random.self_init ();
  let d = Deck.newDeck () in
  Printf.printf "d = %s" (list_to_string id (Deck.toStringList d));

  let (c, d) = Deck.drawCard d in
  Printf.printf "After draw,\nd = %s\nc = %s\n"
    (list_to_string id (Deck.toStringListVerbose d))
    (Deck.Card.toStringVerbose c)

