module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
  val print_bits : t -> unit
end

module type FRACTIONNAL_BITS =
sig
  val bits : int
end

module type MAKE =
  functor (Fb:FRACTIONNAL_BITS) -> FIXED

let pow x n =
  let rec loop n acc =
    if n == 0 then acc else loop (n - 1) (acc * x)
  in
  loop n 1

let float_round f = int_of_float f + (if mod_float f 10.0 >= 0.5 then 1 else 0)

module Make : MAKE =
  functor (Fb : FRACTIONNAL_BITS) ->
  struct
    type t = int
    let m = pow 2 Fb.bits
    let of_float f = float_round (float_of_int m *. f)
    let of_int i = i * m
    let to_float f = float_of_int f /. float_of_int m
    let to_int f = f / m
    let to_string f = string_of_float (to_float f)
    let rec print_bits f =
      if f <> 0 then
        begin
          print_int (f mod 2);
          print_bits (f / 2)
        end
    let zero = 0
    let one = m
    let succ = (+) 1
    let pred = (-) 1
    let gth = (>)
    let lth = (<)
    let gte = (>=)
    let lte = (<=)
    let min = min
    let max = max
    let eqp = (==)
    let eqs = (=)
    let add = (+)
    let sub = (-)
    let mul = ( * )
    let div = (/)
    let rec foreach lb ub f =
      if lte lb ub then (f lb; foreach (succ lb) ub f)
  end

module Fixed2 = Make (struct let bits = 2 end)
module Fixed3 = Make (struct let bits = 3 end)

let () =
  List.iter
    (fun f -> Printf.printf "Fixed3.of_float %f = %s or " f
        (Fixed3.to_string (Fixed3.of_float f));
      (Fixed3.print_bits (Fixed3.of_float f));
      print_char '\n')
    [0.0; 0.2; 3.6; 1.34; 1.6789; 1.240]

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
