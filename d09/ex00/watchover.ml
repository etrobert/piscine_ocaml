type hour = int
let zero = 12
let add x y = (x + y) mod zero
let sub x y = ((x - y) + zero) mod zero
