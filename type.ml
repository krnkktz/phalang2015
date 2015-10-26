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

let rec show_pt = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tlist t = show_pt t ^ " list"
  | Tfun (t, t2) -> show_pt t ^ " -> " ^ show_pt t2
  | Poly s -> "'" ^ s

let rec proof = function
  | Syn.Int n -> Int n, Tint
  | Syn.Bool b -> Bool n, Tbool
  | Syn.List Syn.Nil -> List Nil, Poly "a"
  | Syn.List Syn.Li (x, xs) -> (match proof xs with
    | e, Tlist t -> if proof x = t then List Li (x, xs), Tlist t
    | _ -> raise @@ Error "weird list")
  | Syn.Fun (v, e) ->
      let tv = infer_type v e in
      let ne, te = proof e in
      Fun (v, (ne, te)), Tfun (tv, te)

and infer_type v = function
  | Syn.Var x 
  | Syn.Application (e1, e2) -> in


