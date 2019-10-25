let () =
  print_endline (string_of_int (Watchover.add 2 2));
  print_endline (string_of_int (Watchover.add 4 9));
  print_endline (string_of_int (Watchover.add 0 0));

  print_endline (string_of_int (Watchover.sub 2 2));
  print_endline (string_of_int (Watchover.sub 4 9));
  print_endline (string_of_int (Watchover.sub 0 0))

