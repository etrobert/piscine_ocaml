let ft_sum e lb ub =
  let rec loop e lb ub acc =
    match e, lb, ub, acc with
    | _, lb, ub, acc when lb > ub -> acc
    | e, lb, ub, acc -> loop e (lb + 1) ub (acc +. (e lb))
    in
  if lb > ub then nan else loop e lb ub 0.0
