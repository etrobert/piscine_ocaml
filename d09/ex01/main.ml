let print_proj (p:App.project) =
  print_endline (App.to_string p)

let () =
  print_proj (App.zero);
  let p = ("Upgrade the subject's english level", "failed", -15) in
  print_proj p;
  print_proj (App.success p);
  print_proj (App.fail p);
  print_proj (App.combine App.zero p);
  print_proj (App.combine p p);
  let p2 = "Take a piss", "succeed", 999 in
  print_proj (App.combine p p2)
