(* show.ml *)

open Syn


let show = function
  | Int n -> "int " ^ string_of_int n
  | Bool true -> "bool true"
  | Bool false -> "bool false"
  | Fun _ -> "fun"
  | x -> "shit! " ^ Syn.show x

