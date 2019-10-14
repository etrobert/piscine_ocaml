let rec converges f x n =
  let f_x = f x in
  match f, x, n with
  | _, _, n when n < 0 -> false
  | f, x, _ when x = f_x -> true
  | _, _, 0 -> false
  | f, x, n -> converges f f_x (n - 1)
