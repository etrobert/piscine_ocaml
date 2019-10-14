let rec repeat_x = function
  | n when n < 0 -> "Error"
  | 0 -> ""
  | n -> repeat_x (n - 1) ^ "x"
