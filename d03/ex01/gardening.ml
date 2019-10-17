type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec height = function
  | Nil -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right)

let rec size = function
  | Nil -> 0
  | Node (_, l, r) -> 1 + size l + size r

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
  let t = Node (
      2,
      Node (
        3,
        Node (
          4,
          Nil,
          Nil
        ),
        Node (
          8,
          Nil,
          Nil
        )
      ),
      Nil
    ) in
  print_tree print_int t;
  Printf.printf "size t = %d\n" (size t);
  Printf.printf "height t = %d\n" (height t)
