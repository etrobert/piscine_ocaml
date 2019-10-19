let list_to_string to_string l =
  List.fold_left (fun acc x -> acc ^ to_string x ^ "; ") "[" l ^ "]"

let id x = x

module Card = Deck.Card
(* Card.Color Tests *)
module Color = Card.Color
let () =
  List.iter
    (fun c ->
      print_endline (Color.toString c);
      print_endline (Color.toStringVerbose c))
    Color.all

(* Card.Value Tests *)
module Value = Card.Value
let () =
  List.iter
    (fun v ->
      Printf.printf "value: %d\n" (Value.toInt v);
      Printf.printf "toString: %s\n" (Value.toString v);
      Printf.printf "toStringVerbose: %s\n" (Value.toStringVerbose v);
      Printf.printf "next: %s\n" (Value.toString (Value.next v));
      Printf.printf "previous: %s\n" (Value.toString (Value.previous v));
      print_endline "****************"
      )
    Card.Value.all

(* Card Tests *)
let () =
  Printf.printf "All Spades:\n%s\n"
    (list_to_string Card.toString Card.allSpades);
  Printf.printf "All Hearts:\n%s\n"
    (list_to_string Card.toString Card.allHearts);
  Printf.printf "All Diamonds:\n%s\n"
    (list_to_string Card.toString Card.allDiamonds);
  Printf.printf "All Clubs:\n%s\n"
    (list_to_string Card.toString Card.allClubs);
  Printf.printf "All Cards:\n%s\n"
    (list_to_string Card.toString Card.all);
  let queen_heart = Card.newCard Card.Value.Queen Card.Color.Heart in
  Printf.printf "Card %s has value %s and color %s\n"
    (Card.toStringVerbose queen_heart)
    (Card.Value.toStringVerbose (Card.getValue queen_heart))
    (Card.Color.toStringVerbose (Card.getColor queen_heart));

  (* Compare tests *)
  let eight_spade = Card.newCard Card.Value.T8 Card.Color.Spade in
  Printf.printf "compare %s %s = %d\n"
    (Card.toStringVerbose queen_heart)
    (Card.toStringVerbose eight_spade)
    (Card.compare queen_heart eight_spade);
  let nine_spade = Card.newCard Card.Value.T9 Card.Color.Spade in
  Printf.printf "compare %s %s = %d\n"
    (Card.toStringVerbose eight_spade)
    (Card.toStringVerbose nine_spade)
    (compare eight_spade nine_spade);
  Printf.printf "compare %s %s = %d\n"
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose eight_spade)
    (compare nine_spade eight_spade);

  (* Card.max tests *)
  Printf.printf "max %s %s = %s\n"
    (Card.toStringVerbose eight_spade)
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose (max eight_spade nine_spade));
  Printf.printf "max %s %s = %s\n"
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose eight_spade)
    (Card.toStringVerbose (max nine_spade eight_spade));

  Printf.printf "max %s %s = %s\n"
    (Card.toStringVerbose queen_heart)
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose (max queen_heart nine_spade));
  Printf.printf "max %s %s = %s\n"
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose queen_heart)
    (Card.toStringVerbose (max nine_spade queen_heart));

  (* Card.min tests *)
  Printf.printf "min %s %s = %s\n"
    (Card.toStringVerbose eight_spade)
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose (min eight_spade nine_spade));
  Printf.printf "min %s %s = %s\n"
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose eight_spade)
    (Card.toStringVerbose (min nine_spade eight_spade));
  Printf.printf "min %s %s = %s\n"
    (Card.toStringVerbose queen_heart)
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose (min queen_heart nine_spade));
  Printf.printf "min %s %s = %s\n"
    (Card.toStringVerbose nine_spade)
    (Card.toStringVerbose queen_heart)
    (Card.toStringVerbose (min nine_spade queen_heart));

  (* Card.best tests *)
  let test_best l =
    Printf.printf "best %s = " (list_to_string Card.toString l);
    try
      let b = Card.best l in
      Printf.printf "%s\n"
        (Card.toStringVerbose b)
    with e -> Printf.printf "Exception %s has been catched\n" (Printexc.to_string e)
  in
  test_best [];
  test_best [eight_spade];
  test_best [eight_spade; eight_spade];
  test_best [eight_spade; queen_heart; eight_spade];

  (* Tests isOf *)
  let test_is_of card color =
    Printf.printf "isOf %s %s = %B\n"
      (Card.toStringVerbose card)
      (Card.Color.toStringVerbose color)
      (Card.isOf card color)
  in
  test_is_of eight_spade Card.Color.Diamond;
  test_is_of eight_spade Card.Color.Spade;
  test_is_of eight_spade Card.Color.Club;
  test_is_of eight_spade Card.Color.Heart;

  (* Tests isX *)
  Printf.printf "isSpade %s = %B\n"
    (Card.toStringVerbose eight_spade)
    (Card.isSpade eight_spade);
  Printf.printf "isHeart %s = %B\n"
    (Card.toStringVerbose eight_spade)
    (Card.isHeart eight_spade);
  Printf.printf "isClub %s = %B\n"
    (Card.toStringVerbose eight_spade)
    (Card.isClub eight_spade);
  Printf.printf "isDiamond %s = %B\n"
    (Card.toStringVerbose eight_spade)
    (Card.isDiamond eight_spade)

(* Deck Tests *)
let () =
  Random.self_init ();
  let d = Deck.newDeck () in
  Printf.printf "d = %s" (list_to_string id (Deck.toStringList d));

  let (c, d) = Deck.drawCard d in
  Printf.printf "After draw,\nd = %s\nc = %s\n"
    (list_to_string id (Deck.toStringListVerbose d))
    (Deck.Card.toStringVerbose c)
