type project = string * string * int

let zero = "", "", 0

let combine (n1, _, g1) (n2, _, g2) =
  let grade = (g1 + g2) / 2 in
  (n1 ^ n2, (if grade >= 80 then "succeed" else "failed"), grade)

let fail (n, _, _) = (n, "failed", 0)

let success (n, _, _) = (n, "succeed", 80)

let to_string (n, st, g) =
  "Project \"" ^ n ^ "\" (" ^ string_of_int g ^ " -> " ^ st ^ ")"
