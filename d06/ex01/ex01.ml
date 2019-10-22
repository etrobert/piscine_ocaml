let pow x n =
  let rec loop n acc =
    if n == 0 then acc else loop (n - 1) (acc * x)
  in
  loop n 1

(*let () =
  List.iter (fun (x, n) -> Printf.printf "%d ^ %d = %d\n" x n (pow x n))
    [
      (1, 2);
      (1, 5);
      (2, 2);
      (3, 2);
      (4, 2);
      (6, 2);
      (4, 4);
    ]
*)

module StringHash =
struct
  let fold_left f base_acc s =
    let len = String.length s in
    let rec loop i acc =
      if i = len then acc
      else loop (i + 1) (f acc (String.get s i))
    in
    loop 0 base_acc
  type t = string
  let equal s1 s2 = s1 = s2
  let hash s =
    let p = 31 in
    let m = pow 10 9 + 9 in
    (fst (fold_left (fun (v, cur_p) c ->
         (v + int_of_char c * cur_p, cur_p * p)) (0, 1) s)) mod m
end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
