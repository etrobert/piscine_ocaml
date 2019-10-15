let ft_is_palindrome s =
  let len = String.length s in
  let rec ft_is_palindrome_r s i =
    if i >= (len / 2) then true
    else if String.get s i <> (String.get s (len - 1 - i)) then false
    else ft_is_palindrome_r s (i + 1)
    in
  ft_is_palindrome_r s 0

let () =
  let test_ft_is_palindrome s output =
    let r = ft_is_palindrome s in
    if r = output
    then Printf.printf "SUCCESS: ft_is_palindrome \"%s\" = %B\n" s r
    else Printf.printf
      "FAILURE: ft_is_palindrome \"%s\" = %B <> %B\n" s r output
    in
  test_ft_is_palindrome "radar" true;
  test_ft_is_palindrome "raar" true;
  test_ft_is_palindrome "madam" true;
  test_ft_is_palindrome "bonbon" false;
  test_ft_is_palindrome "car" false;
  test_ft_is_palindrome "" true
