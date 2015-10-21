-- booleans --

let not a = if a then false else true in

-- integers --

let min a b = if a < b then a else b in
let max a b = if a > b then a else b in
let succ a = a + 1 in
let pred a = a - 1 in
let abs a = if a > 0 then a else 0 - a in
let fact n = if n < 2 then 1 else n * fact (pred n) in

-- lists --

let length x = if null x then 0 else succ (length (tail x)) in

let nth n x =
    if n == 0 then head x else nth (n - 1) (tail x) in

let test_list =
  not true : min 1 2 : max 1 2 : succ 1
  : pred 1 : abs (0 - 3) : fact 5 : nil in

length test_list : nth 4 test_list : nil

