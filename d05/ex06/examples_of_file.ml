let count_lines ic =
  let i = ref 0 in
  try
    while true do
      ignore (input_line ic);
      incr i
    done;
    0
  with End_of_file -> !i

type radar = float array * string

let radar_of_string s : radar =
  let values = String.split_on_char ',' s in
  let arr = Array.of_list values in
  let s = Array.get arr (Array.length arr - 1) in
  Array.map float_of_string (Array.sub arr 0 (Array.length arr - 1)), s

let load_radars ic : radar list =
  let l = ref [] in
  (try
     while true do
       let s = input_line ic in
       l := radar_of_string s::!l
     done
   with End_of_file -> ());
  !l

let examples_of_file filename =
  let ic = open_in filename in
  let radars = load_radars ic in
  close_in ic;
  radars

(* Tests *)

let print_radar (stats, cl) =
  Printf.printf "Radars class %s\nStats : " cl;
  Array.iter (fun x -> Printf.printf "%f, " x) stats;
  print_char '\n'

let () =
  List.iter print_radar (examples_of_file (Array.get Sys.argv 1))
