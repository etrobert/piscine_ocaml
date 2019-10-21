let my_sleep () = Unix.sleep 1

let () =
  try
    for i = 1 to (int_of_string (Array.get Sys.argv 1)) do my_sleep () done
  with e -> ()
