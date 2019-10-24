class doctor (i_name, i_age, i_sidekick) =
  object
    initializer print_endline ("The doctor " ^ i_name ^ " has been born.")
    val name : string = i_name
    val age : int = i_age
    val sidekick = i_sidekick
    val hp : int = 100
    method to_string =
      "doctor(" ^
      "name=" ^ name ^ ", " ^
      "age=" ^ string_of_int age ^ ", " ^
      "sidekick=" ^ sidekick#to_string ^ ", " ^
      "hp=" ^ string_of_int hp ^
      ")"
    method talk = print_endline "Hi! I'm the Doctor!"
    method travel_in_time start arrival =
      print_endline "        ___
_______(_@_)_______
| POLICE      BOX |
|_________________|
 | _____ | _____ |
 | |###| | |###| |
 | |###| | |###| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ |$_____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 |       |       |
 *****************";
      {< age = age + arrival - start>}
    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhii Whiiiiwhiiiwhii Whiiiiwhiiiwhii"
    method private regenerate = {< hp = 100 >}

  end
