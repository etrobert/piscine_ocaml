let fibonacci n =
  let rec fibonacci_r a b n =
    match a,b,n with
    | _, b, 0 -> b
    | a, b, n -> fibonacci_r (a + b) a (n - 1)
    in
  if n < 0 then (-1)
  else fibonacci_r 1 0 n
