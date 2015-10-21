(* show.ml *)

open Syn


let rec show = function
  | Int n -> "int " ^ string_of_int n
  | Bool true -> "bool true"
  | Bool false -> "bool false"
  | Fun _ -> "fun"
  | List Nil -> "nil"
  | List Li (x, xs) -> show x ^ " : " ^ show @@ List xs
  | x -> "shit! " ^ Syn.show x

