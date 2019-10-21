(* Exercise 05 *)
let eu_dist a b =
  sqrt (Array.fold_left (+.) 0.0 (Array.map2 (fun x y -> (x -. y) *. (x -. y)) a b))

(* Exercise 06 *)
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

let radar_stats (r:radar) = fst r
let radar_class (r:radar) = snd r

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

(* Exercise 07 *)

let radar_dist r1 r2 = eu_dist (radar_stats r1) (radar_stats r2)

let one_nn l (r:radar) : string =
  match l with
  | [] -> invalid_arg "List should not be empty"
  | hd::tl -> radar_class
                (fst (List.fold_left
                        (fun (top_rad, top_dist) rad ->
                           let dist = radar_dist rad r in
                           if dist < top_dist then (rad, dist) else top_rad, top_dist)
                        (hd, radar_dist r hd)
                        tl))

(* Tests *)

let print_radar (stats, cl) =
  Printf.printf "Radars class %s\nStats : " cl;
  Array.iter (fun x -> Printf.printf "%f, " x) stats;
  print_char '\n'

let () =
  match examples_of_file "ionosphere.test.csv" with
  | [] -> print_endline "Empty file"
  | hd::tl -> Printf.printf "Closest class from first radar is %s\n" (one_nn tl hd)
