let () =
  let jean = new People.people ("Jean") in
  print_endline jean#to_string;
  jean#talk;
  jean#die
