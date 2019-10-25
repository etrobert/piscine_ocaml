let try_print_int k =
  Try.bind k (fun x -> Try.return (print_endline (string_of_int x)))

let safe_divide a =
  function
  | 0 -> Try.Failure (Invalid_argument "Cannot divie by 0")
  | b -> Try.return (a / b)

let (_:unit Try.t) =
  Try.recover
    (try_print_int (safe_divide 2 3))
    (fun e -> Try.return (print_endline (Printexc.to_string e)))

let (_:unit Try.t) =
  try_print_int
    (Try.bind
       (Try.bind
          (safe_divide 10 6)
          (fun x -> Try.return (x + 9)))
       (fun x -> Try.return (x - 3)))

let (_:unit Try.t) =
  try_print_int
    (Try.recover
       (Try.bind
          (Try.bind
             (safe_divide 10 0)
             (fun x -> Try.return (x + 9)))
          (fun x -> Try.return (x - 3)))
       (fun e ->
          print_endline
            ("Pfiouu got it last second from: " ^ Printexc.to_string e);
          Try.return (-1)))

let (_:unit Try.t) =
  Try.recover
    (try_print_int
       (Try.filter
          (safe_divide 9 3)
          (fun x -> x < 0)))
    (fun e -> Try.return
        (print_endline ("An error occured: " ^ Printexc.to_string e)))


let (_:unit Try.t) =
  try_print_int (Try.bind (safe_divide 4 2) (safe_divide 3))

let (_:unit Try.t) =
  try_print_int (Try.flatten (Try.return (Try.return 3)))
