let () =
  let h = new Atom.hydrogen in
  let c = new Atom.carbon in
  Printf.printf "%s = %s ? %B\n" h#to_string c#to_string (h#equals c);
  Printf.printf "%s = %s ? %B\n" c#to_string c#to_string (c#equals c)
