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

let rec eval names =
  function
  | Application (exp1, exp2) -> (match eval names exp1 with
    | Fun (_, Builtin f) -> f @@ eval names exp2
    | Fun (n, f) -> let v = eval names exp2 in
      eval ((n, v) :: names) @@ closure n v f
    | _ -> raise @@ Error "can't apply that!")
  | Var s -> get_var s names
  | Cond (c, i, e) -> (match eval names c with
    | Bool true -> eval names i
    | Bool false -> eval names e
    | _ -> raise @@ Error "condition is not boolean")
  | Let (n, v, c) ->
      eval names @@ Application (Fun (n, c), v)
  | Int n -> Int n
  | Bool b -> Bool b
  | Fun (n, e) -> Fun (n, e)
  | Builtin f -> Builtin f
  | List x -> List x and

  closure n v = function
    | Application (e1, e2) ->
        Application (closure n v e1, closure n v  e2)
    | Var s when s = n -> v
    | Cond (c, i, e) ->
      Cond (closure n v c, closure n v i, closure n v e)
    | Let (n1, v1, c) when n1 = n -> Let (n1, closure n v v1, c)
    | Let (n1, v1, c) -> Let (n1, closure n v v1, closure n v c)
    | Fun (n1, e) when n1 = n -> Fun (n1, e)
    | Fun (n1, e) -> Fun (n1, closure n v e)
    | e -> e

