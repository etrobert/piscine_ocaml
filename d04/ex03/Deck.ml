module Card =
  struct
  module Value =
    struct
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
    end

  module Color =
    struct
      type t = Spade | Heart | Diamond | Club

      let all = [Spade; Heart; Diamond; Club]

      let toString = function
        | Spade -> "S"
        | Heart -> "H"
        | Diamond -> "D"
        | Club -> "C"

      let toStringVerbose = function
        | Spade -> "Spade"
        | Heart -> "Heart"
        | Diamond -> "Diamond"
        | Club -> "Club"
    end

  type t = Card of (Value.t * Color.t)

  let newCard v c = Card (v, c)

  let allSpades = List.map (fun x -> newCard x Color.Spade) Value.all
  let allHearts = List.map (fun x -> newCard x Color.Heart) Value.all
  let allDiamonds = List.map (fun x -> newCard x Color.Diamond) Value.all
  let allClubs = List.map (fun x -> newCard x Color.Club) Value.all

  (* The order does not matter; so using fold_left allows for faster result *)
  let all = List.fold_left
    (fun acc v ->
      List.fold_left (fun acc2 c -> (newCard v c)::acc2) acc Color.all)
    [] Value.all

  let getValue (Card (v, _)) = v
  let getColor (Card (_, c)) = c

  let toString (Card (v, c)) = Value.toString v ^ Color.toString c
  let toStringVerbose (Card (v, c)) =
    Printf.sprintf
      "Card(%s, %s)" (Value.toStringVerbose v) (Color.toStringVerbose c)

  let compare (Card (a, _)) (Card (b, _)) = Value.toInt a - Value.toInt b

  let max a b = if compare a b >= 0 then a else b
  let min a b = if compare a b <= 0 then a else b

  let best = function
    | [] -> invalid_arg "List should not be empty"
    | hd::tl -> List.fold_left max hd tl

  let isOf (Card (_, c)) color = c = color

  let isSpade c = isOf c Color.Spade
  let isHeart c = isOf c Color.Heart
  let isDiamond c = isOf c Color.Diamond
  let isClub c = isOf c Color.Club
end

type t = Card.t list

let newDeck () =
  List.sort
    (fun x y -> (if Random.bool () then 1 else (-1)))
    Card.all

let toStringList = List.map Card.toString
let toStringListVerbose = List.map Card.toStringVerbose

let drawCard = function
  | [] -> failwith "Deck is empty"
  | hd::tl -> (hd, tl)
