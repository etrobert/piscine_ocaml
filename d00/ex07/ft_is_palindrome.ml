let ft_is_palindrome s =
  let len = String.length s in
  let rec ft_is_palindrome_r s i =
    match s, i with
    | s, i when i >= (len / 2) -> true
    | s, i when String.get s i <> (String.get s (len - 1 - i)) -> false
    | s, i -> ft_is_palindrome_r s (i + 1)
    in
  ft_is_palindrome_r s 0

let () =
  let test_pal s =
    Printf.printf  "%s is a palindrome: %B\n" s (ft_is_palindrome s)
    in
  test_pal "radar";
  test_pal "madam";
  test_pal "bonbon";
  test_pal "car";
  test_pal ""
