let rec iter f = function
  | [] -> ()
  | hd::tl -> f hd; iter f tl

let () =
  iter
    (fun c ->
      print_endline (Color.toString c);
      print_endline (Color.toStringVerbose c))
    Color.all
