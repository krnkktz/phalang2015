(* ulambda.ml *)

exception Error of string

type lexpr =
  | App of lexpr * lexpr
  | Var of string
  | Fun of string * lexpr

let rec compile_int = function
  | 0 -> Var "x"
  | n -> App (Var "f", compile_int @@ pred n)

let rec compile = function
  | Syn.Application (e1, e2) -> App (compile e1, compile e2)
  | Syn.Cond (p, t, f) -> App (App (compile p, compile t), compile f)
  | Syn.Fun (s, e) -> Fun (s, compile e)
  | Syn.Bool true -> Fun ("x", Fun ("y", Var "x"))
  | Syn.Bool false -> Fun ("x", Fun ("y", Var "y"))
  | Syn.Var "and" ->
      Fun ("p", Fun ("q", App (App (Var "p", Var "q"), Var "p")))
  | Syn.Var s -> Var s
  | Syn.Builtin _ -> raise @@ Error "this builtin is not implemented"
  | Syn.Int n -> Fun ("f", Fun ("x", compile_int n))
  | Syn.List n -> raise @@ Error "lists are not implemented"

let rec show = function
  | App (e1, e2) -> show e1 ^ " " ^ show e2
  | Var s -> s
  | Fun (s, e) -> "(λ" ^ s ^ "." ^ show e ^ ")"

let rec replace n e = function
  | App (e1, e2) -> App (replace n e e1, replace n e e2)
  | Var s when s = n -> e
  | Fun (s, e1) when s <> n -> Fun (s, replace n e e1)
  | e -> e

let rec reduce = function
  | App (e1, e2) -> (match reduce e1 with
    | Fun (s, e3) -> replace s e2 e3
    | e -> App (e, reduce e2))
  | e -> e


