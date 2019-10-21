let jokes = Array.init 3
    (function 0 -> "Il etait une fois, un pigeon."
            | 1 -> "C'est l'histoire d'un aveugle qui rentre chez lui."
            | 2 -> "You're beautiful as a woman and smart as a man."
            | _ -> "")

let () =
  Array.iter print_endline jokes
