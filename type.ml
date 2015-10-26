(* type.ml *)

type pt =
  | Tint
  | Tbool
  | Tlist of t
  | Tfun of t * t
  | Poly of string

type t =
  | Application of (t * t)
  | Var of string
  | Cond of (t * t * t)
  | Int of int
  | Bool of bool
  | Fun of (string * t)
  | Builtin of (t -> t)
  | List of li

and li =
  | Nil
  | Li of t * li

type typedast = t * pt

let rec proof_types = function
  | Syn.Int n -> Int n, Tint
  | Syn.Bool b -> Bool n, Tbool
  | Syn.List Syn.Nil -> List Nil, Poly "a"
  | Syn.List Syn.Li (x, xs) -> (match proof_types xs with
    | e, Tlist t -> if proof_types x = t then List Li (x, xs), Tlist t
    | _ -> raise @@ Error "weird list")

and infer_type v = function
  | Syn.Var x 
  | Syn.Application (e1, e2) -> in


