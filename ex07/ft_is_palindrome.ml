let ft_is_palindrome s =
  (*
    Split the string in half
    Reverse second half
    Compare the halves
  *)
  let split_string s =
    let substring_size = ((String.length s) / 2) in
    (String.sub s 0 substring_size,
    String.sub s (String.length s - substring_size) substring_size)
    in
  let string_reverse s =
    let len = String.length s in
    String.init len (fun i -> s.[len - i - 1])
    in
  let ss = split_string s in
  String.equal (fst ss) (string_reverse (snd ss))

let () =
  let test_pal s =
    Printf.printf  "%s is a palindrome: %B\n" s (ft_is_palindrome s)
    in
  test_pal "radar";
  test_pal "madam";
  test_pal "bonbon";
  test_pal "car";
  test_pal ""
