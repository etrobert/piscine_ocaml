let rec ackermann m n =
  match m,n with
  | m, n when m < 0 || n < 0 -> -1
  | 0, n -> n + 1
  | m, 0 -> ackermann (m - 1) 1
  | m, n -> ackermann (m - 1) (ackermann m (n - 1))
