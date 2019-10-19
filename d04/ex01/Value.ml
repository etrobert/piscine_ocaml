type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

let toInt = function
  | T2 -> 1
  | T3 -> 2
  | T4 -> 3
  | T5 -> 4
  | T6 -> 5
  | T7 -> 6
  | T8 -> 7
  | T9 -> 8
  | T10 -> 9
  | Jack -> 10
  | Queen -> 11
  | King -> 12
  | As -> 13

let fromInt = function
  | 1 -> T2
  | 2 -> T3
  | 3 -> T4
  | 4 -> T5
  | 5 -> T6
  | 6 -> T7
  | 7 -> T8
  | 8 -> T9
  | 9 -> T10
  | 10 -> Jack
  | 11 -> Queen
  | 12 -> King
  | 13 -> As
  | _ -> failwith "Invalid parameter"

let toString = function
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | As -> "A"

let toStringVerbose = function
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | As -> "As"

let next v = fromInt ((((toInt v) - 1) + 1) mod 13 + 1)

let previous v = fromInt (((toInt v) - 1 + 13 - 1) mod 13 + 1)
