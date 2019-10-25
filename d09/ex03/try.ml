type 'a t = Success of 'a | Failure of exn

let return x = Success x

let bind x f =
  match x with
  | Success k -> (try f k with e -> Failure e)
  | Failure e -> Failure e

let recover x f = match x with
  | Failure e -> f e
  | _ -> x

exception UnmatchedFilter

let filter x f = match x with
  | Success v when not (f v) -> Failure UnmatchedFilter
  | o -> o

let flatten = function
  | Success x -> x
  | Failure e -> Failure e
