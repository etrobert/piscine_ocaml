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

let rec insert_in_sorted_list compare l e =
  match l with
  | [] -> [e]
  | hd::tl -> if compare e hd < 0.0
    then e::hd::tl
    else hd::insert_in_sorted_list compare tl e

let k_nn l k (r:radar) =
  let rec update_class class_list (radar, dist) : (string * int * float) list =
    match class_list with
    | [] -> [(radar_class radar, 1, dist)]
    | (class_name, class_sum, dist_sum) as hd::tl ->
      if class_name = radar_class radar
      then (class_name, class_sum + 1, dist_sum +. dist)::tl
      else hd::update_class class_list (radar, dist)
  in
  let choose_class l : string =
    let summed_list = List.fold_left update_class [] l in
    match summed_list with
    | [] -> failwith "There are no classes to choose from"
    | hd::tl ->
      let (class_name, _, _) =
        List.fold_left (fun top cur ->
            match top, cur with
            | (_, top_class_sum, _), ((_, class_sum, _) as cur) when class_sum < top_class_sum -> cur
            | (_, top_class_sum, top_dist_sum), ((_, class_sum, dist_sum) as cur)
              when class_sum = top_class_sum && dist_sum < top_dist_sum -> cur
            | top, _ -> top) hd tl
      in
      class_name
  in
  List.fold_left (fun acc radar_dist -> update_class acc radar_dist) []
    match l with
    | [] -> invalid_arg "List should not be empty"
    | hd::tl ->
      (List.fold_left
         (fun l rad ->
            let dist = radar_dist rad r in
            let new_l = insert_in_sorted_list (fun (_, a) (_, b) -> b -. a) l (rad, dist) in
            if List.length l < k
            then new_l
            else List.tl new_l)
         [(hd, radar_dist r hd)]
         tl)

(* Tests *)

let print_radar (stats, cl) =
  Printf.printf "Radars class %s\nStats : " cl;
  Array.iter (fun x -> Printf.printf "%f, " x) stats;
  print_char '\n'

let () =
  match examples_of_file "ionosphere.test.csv" with
  | [] -> print_endline "Empty file"
  | hd::tl ->
    Printf.printf
      "Closest class from first radar is %s. Correct answer was %s\n"
      (one_nn tl hd)
      (radar_class hd)
