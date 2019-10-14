let ft_string_all p s =
  let rec ft_string_all_r p s i =
    match p,s,i with
    | _, s, i when i == String.length s -> true
    | p, s, i when p (String.get s i) == false -> false
    | p, s, i -> ft_string_all_r p s (i + 1)
    in
    ft_string_all_r p s 0

let () =
  let is_digit c = c >= '0' && c <= '9' in
  Printf.printf "%B\n" (ft_string_all is_digit "0123456789");
  Printf.printf "%B\n" (ft_string_all is_digit "012345A789");
