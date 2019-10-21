type 'a ft_ref = { mutable value: 'a }

let return v = { value = v }

let get r = r.value

let set r v = r.value <- v

let bind r f = f r.value

let () =
  let r = return 4 in
  Printf.printf "r equals %d\n" (get r);
  set r 5;
  Printf.printf "set r 5\n";
  Printf.printf "r now equals %d\n" (get r);
  let times_two x = return (x * 2) in
  Printf.printf "get $ bind r times_two = %d\n" (get (bind r times_two))
