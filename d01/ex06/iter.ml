let rec iter p x n =
  match p, x, n with
  | _, _, n when n < 0 -> -1
  | _, x, 0 -> x
  | p, x, n -> iter p (p x) (n - 1)
