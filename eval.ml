(* eval.ml *)

open Syn

exception Error of string


let rec get_var s = function
  | [] -> raise @@ Error ("unknown builtin " ^ s)
  | (x, v) :: _ when x = s -> v
  | _ :: xs -> get_var s xs

let rec closure n v = function
  | Application (f, x) -> Application (closure n v f, closure n v x)
  | Cond (p, t, f) -> Cond (closure n v p, closure n v t, closure n v f)
  | Var s when s = n -> v
  | Fun (x, e) when x <> n -> Fun (x, closure n v e)
  | e -> e

let rec eval = function
(*  print_string @@ Syn.show t ; print_newline () ; print_newline () ; match t
 *  with *)
  | Application (f, x) -> (match eval f with
    | Fun (_, Builtin f) -> f @@ eval x
    | Fun (s, e) -> eval @@ closure s (eval x) e
    | t -> raise @@ Error ("not a function but a " ^ Syn.show t))
  | Cond (p, t, f) -> (match eval p with
    | Bool true -> eval t
    | Bool false -> eval f
    | _ -> raise @@ Error "not a predicate")
  | Var s -> get_var s Builtin.builtin
  | e -> e

