(* eval.ml *)

open Syn

exception Error of string

let rec get_var s = function
  | (n, v) :: _ when n = s -> v
  | _ :: xs -> get_var s xs
  | [] -> raise @@ Error ("unknown variable " ^ s)

let rec show_namespace = function
  | [] ->  ""
  | (n, v) :: xs -> "var " ^ n ^ ": " ^ Syn.show v ^ "\n" ^ show_namespace xs

let rec eval =
  function
  | Application (exp1, exp2) -> (match eval exp1 with
    | Fun (_, Builtin f) -> f @@ eval exp2
    | Fun (n, f) -> let v = eval exp2 in
      eval @@ closure n v f
    | _ -> raise @@ Error "can't apply that!")
  | Var s -> get_var s Builtin.builtin
  | Cond (c, i, e) -> (match eval c with
    | Bool true -> eval i
    | Bool false -> eval e
    | _ -> raise @@ Error "condition is not boolean")
  | Int n -> Int n
  | Bool b -> Bool b
  | Fun (n, e) -> Fun (n, e)
  | Builtin f -> Builtin f
  | List x -> List x and

  closure n v = function
    | Application (e1, e2) ->
      Application (closure n v e1, closure n v  e2)
    | Cond (c, i, e) ->
      Cond (closure n v c, closure n v i, closure n v e)
    | Fun (n1, e) when n1 <> n -> Fun (n1, closure n v e)
    | Var s when s = n -> v
    | e -> e

