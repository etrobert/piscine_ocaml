let rec repeat_string ?(str = "x") n =
  match str,n with
  | _,n when n < 0 -> "Error"
  | _,0 -> ""
  | s,n -> repeat_string ~str:str (n - 1) ^ s
