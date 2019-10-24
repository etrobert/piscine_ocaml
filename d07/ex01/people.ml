class people i_name =
  object
    initializer print_endline (i_name ^ " has been born")
    val name : string = i_name
    val hp : int = 100
    method to_string : string =
      "people(" ^
      "name=" ^ name ^ ", " ^
      "hp=" ^ string_of_int hp ^
      ")"
    method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor ?")
    method die = print_endline "Aaaarghh!"
  end
