let eu_dist a b =
  sqrt (Array.fold_left (+.) 0.0 (Array.map2 (fun x y -> (x -. y) *. (x -. y)) a b))

let () =
  Printf.printf "eu_dist (-1,2,3) (4,0,-3) = %f\n"
    (eu_dist
       (Array.init 3 (function 0 -> -1.0 | 1 -> 2.0 | 2 -> 3.0 | _ -> invalid_arg ""))
       (Array.init 3 (function 0 -> 4.0 | 1 -> 0.0 | 2 -> -3.0 | _ -> invalid_arg "")))
