type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec is_bst = function
  | Nil -> true
  | Node (x, (Node (lv, _, _)), _) when lv > x -> false
  | Node (x, _, (Node (rv, _, _))) when rv < x -> false
  | Node (_, l, r) -> is_bst l && is_bst r

let is_perfect =
  let rec loop = function
    | Node (_, Nil, Nil) -> 1
    | Node (_, (Node (_, _, _) as l), (Node (_, _, _) as r)) ->
      let lh = loop l
      and rh = loop r in
      if lh = -1 || rh = -1 || rh <> lh
      then -1
      else rh + 1
    | _ -> (-1)
  in
  function | Nil -> true | t -> loop t <> -1

let is_balanced t =
  let rec loop = function
    | Nil -> 0
    | Node (_, l, r) ->
      let lh = loop l
      and rh = loop r in
      if lh = -1 || rh = -1 || abs (rh - lh) > 1
      then -1
      else max lh rh + 1
  in
  loop t <> -1

let rec search_bst v t =
  match t with
  | Nil -> false
  | Node (x, l, _) when x < v -> search_bst v l
  | Node (x, _, r) when x > v -> search_bst v r
  | Node (x, _, _) -> true

(* Tests *)

let rec repeat f = function 0 -> () | n -> f (); repeat f (n - 1)

let print_tree printer t =
  let print_pre pre = repeat (fun () -> print_char '-') pre in
  let rec loop pre t =
    match t with
    | Nil -> ()
    | Node (x, left, right) ->
      begin
        print_pre pre;
        printer x;
        print_char '\n';
        loop (pre + 1) left;
        loop (pre + 1) right
      end
  in
  loop 0 t

let () =
  let empty = Nil in
  let not_bst = Node (
      1,
      Node (2, Nil, Nil),
      Nil
    ) in

  let bst = Node (
      1,
      Node (0, Nil, Nil),
      Nil
    ) in

  let big_perfect_bst = Node (
      5,
      Node (
        3,
        Node (2, Nil, Nil),
        Node (4, Nil, Nil)
      ),
      Node (
        7,
        Node (6, Nil, Nil),
        Node (8, Nil, Nil)
      )
    ) in

  let not_balanced = Node (
      1,
      Node ( 2,
             Node (3, Nil, Nil),
             Nil
           ),
      Nil
    ) in

  let test_is_bst name tree =
    print_tree print_int tree;
    print_string (name ^ " is " ^ (if is_bst tree then "" else "not ") ^ "a bst.\n") in

  test_is_bst "empty" empty;
  test_is_bst "bst" bst;
  test_is_bst "not_bst" not_bst;
  test_is_bst "big_perfect_bst" big_perfect_bst;

  let test_is_perfect name tree =
    print_tree print_int tree;
    print_string (name ^ " is " ^ (if is_perfect tree then "" else "not ") ^ "perfect.\n") in

  test_is_perfect "empty" empty;
  test_is_perfect "bst" bst;
  test_is_perfect "big_perfect_bst" big_perfect_bst;

  let test_is_balanced name tree =
    print_tree print_int tree;
    print_string (name ^ " is " ^ (if is_balanced tree then "" else "not ") ^ "balanced.\n") in

  test_is_balanced "empty" empty;
  test_is_balanced "bst" bst;
  test_is_balanced "big_perfect_bst" big_perfect_bst;
  test_is_balanced "not_balanced" not_balanced;

  let test_search_bst name tree item =
    print_tree print_int tree;
    print_string (string_of_int item ^ " is " ^ (if search_bst item tree then "" else "not ") ^ "in " ^ name ^ ".\n") in

  test_search_bst "empty" empty 2;
  test_search_bst "bst" bst 1;
  test_search_bst "big_perfect_bst" big_perfect_bst 1;
  test_search_bst "not_balanced" not_balanced 1

