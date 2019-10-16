let gray n =
  let compute_gray n =
    (* Following functions are the same as in the standard library *)
    let rec map f l =
      match l with
      | [] -> []
      | hd::tl -> (f hd)::(map f tl)
    in
    let rev_map f l =
      let rec loop f l acc =
        match l with
        | [] -> acc
        | hd::tl -> loop f tl ((f hd)::acc)
      in
      loop f l []
    in
    let id x = x in
    let append l1 l2 =
      let rec loop f l1 l2 =
        match l1 with
        | [] -> f l2
        | hd::tl -> loop (fun l -> f (hd::l)) tl l2
      in
      loop id l1 l2
    in
    (* End of standard library functions *)
    let rec loop f n =
      if n = 1 then f [[0];[1]]
      else loop
          (fun l ->
             f (append (map (fun x -> 0::x) l) (rev_map (fun x -> 1::x) l)))
          (n - 1)
    in
    if n <= 0 then [] else loop id n
  in
  let rec print_gray g =
    (* iter is from the standard List module *)
    let rec iter f l =
      match l with
      | [] -> ()
      | hd::tl -> f hd; iter f tl
    in
    iter (fun e -> (iter (fun x -> print_int x) e); print_char ' ') g;
    print_char '\n'
  in
  print_gray (compute_gray n)

let () =
  gray (-1);
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4
