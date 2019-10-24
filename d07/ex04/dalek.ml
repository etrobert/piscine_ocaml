(* Generates an int in the range [lb;up[ *)
let random_int_between lb up =
  Random.int (up - lb) + lb

let generate_sequence () =
  let generate_char () = char_of_int (random_int_between 32 127) in
  String.init 3 (fun _ -> generate_char ())

class dalek =
  object (self)
    val name = "Dalek" ^ generate_sequence ()
    val hp = 100
    val mutable shield = true
    method to_string =
      "dalek(" ^
      "name=" ^ name ^ ", " ^
      "hp=" ^ string_of_int hp ^ ", " ^
      "shield=" ^ string_of_bool shield ^ ", " ^
      ")"

    method talk = print_endline
        (match Random.int 4 with
         | 0 -> "Explain! Explain!"
         | 1 -> "Exterminate! Exterminate!"
         | 2 -> "I obey!"
         | 3 -> "You are the Doctor! You are the enemy of the Daleks!"
         | _ -> failwith "unexpected random number")

    method private update_shield = shield <- not shield
    method exterminate (p:People.people) = self#update_shield; p#die
    method die = print_endline "Emergency Temporal Shift!"
  end
