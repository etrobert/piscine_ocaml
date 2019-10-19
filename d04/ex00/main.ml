let () =
  List.iter
    (fun c ->
      print_endline (Color.toString c);
      print_endline (Color.toStringVerbose c))
    Color.all
