let count_lines ic =
  let i = ref 0 in
  try
    while true do
      ignore (input_line ic);
      incr i
    done;
    0
  with End_of_file -> !i

let store_jokes arr ic =
  try
    let i = ref 0 in
    while true do
      Array.set arr !i (input_line ic);
      incr i
    done
  with End_of_file -> ()


let jokes filename =
  let ic = open_in filename in
  let jokes_array = Array.make (count_lines ic) "" in
  seek_in ic 0;
  store_jokes jokes_array ic;
  Array.iter print_endline jokes_array;
  close_in ic

let () =
  jokes (Array.get Sys.argv 1)
