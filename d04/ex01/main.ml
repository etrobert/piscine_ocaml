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
    Value.all
