let sum = (+.)

let () =
  List.iter (fun (a, b) -> Printf.printf "sum %f %f = %f\n" a b (sum a b))
    [0.0, 0.0;
     1.0, 2.0;
     -1.0, 4.0;
     nan, 2.0]
