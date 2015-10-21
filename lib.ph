-- integers --

let min a b = if a < b then a else b in

let max a b = if a > b then a else b in

let not a = if a then false else true in

let succ a = a + 1 in

let pred a = a - 1 in

let abs a = if a > 0 then a else 0 - a in


-- lists --

let length x = if x == nil then 0 else succ (length (tail x)) in

let nth =
  let h n x =
    if n = 0 then head x else h (succ n) (tail x) in
  h 0 in

min 1 2 : max 1 2 : not true : succ 1 : pred 1 : abs (0 - 3) : nil
