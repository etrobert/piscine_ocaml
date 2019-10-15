let rec hfs_f n =
  if n < 0 then -1
  else if n = 0 then 1
  else n - hfs_m (hfs_f (n - 1))

and hfs_m n =
  if n < 0 then -1
  else if n = 0 then 0
  else n - hfs_f (hfs_m (n - 1))

let () =
  let test_m (input, output) =
    let r = hfs_m input in
    if r = output
    then Printf.printf "SUCCESS: hfs_m %d = %d\n" input r
    else
      Printf.printf "FAILURE: hfs_m %d = %d <> %d\n" input r output
    in
  let tests_m = [
    0, 0;
    4, 2;
  ] in
  List.iter test_m tests_m;

  let test_f (input, output) =
    let r = hfs_f input in
    if r = output
    then Printf.printf "SUCCESS: hfs_f %d = %d\n" input r
    else
      Printf.printf "FAILURE: hfs_f %d = %d <> %d\n" input r output
    in
  let tests_f = [
    0, 1;
    4, 3;
  ] in
  List.iter test_f tests_f
