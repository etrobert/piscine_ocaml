class virtual atom (name, symbol, atomic_number) =
  object
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = atomic_number
    method to_string =
      "atom(" ^
      "name=" ^ name ^ ", " ^
      "symbol=" ^ symbol ^ ", " ^
      "atomic_number=" ^ string_of_int atomic_number ^
      ")"
    method equals (a:atom) =
      a#name = name && a#symbol = symbol && a#atomic_number = atomic_number
  end

class hydrogen =
  object
    inherit atom ("Hydrogen", "H", 1)
  end

class carbon =
  object
    inherit atom ("Carbon", "C", 6)
  end

class nitrogen =
  object
    inherit atom ("Nitrogen", "N", 7)
  end

class oxygen =
  object
    inherit atom ("Oxygen", "O", 8)
  end
