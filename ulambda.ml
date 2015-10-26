(* ulambda.ml *)

exception Error of string

type lexpr =
  | App of lexpr * lexpr
  | Var of string
  | Fun of string * lexpr

let rec compile_int = function
  | 0 -> Var "x"
  | n -> App (Var "f", compile_int @@ pred n)

let rec show_int f = function
  | Var "x" -> 1
  | App (Var f1, e) when f1 = f -> succ (show_int f e)
  | _ -> raise @@ Error "not a number"

let rec compile = function
  | Syn.Application (e1, e2) -> App (compile e1, compile e2)
  | Syn.Cond (p, t, f) -> App (App (compile p, compile t), compile f)
  | Syn.Fun (s, e) -> Fun (s, compile e)
  | Syn.Bool true -> Fun ("x", Fun ("y", Var "x"))
  | Syn.Bool false -> Fun ("x", Fun ("y", Var "y"))
  | Syn.Var "and" ->
      Fun ("p", Fun ("q", App (App (Var "p", Var "q"), Var "p")))
  | Syn.Var "or" ->
      Fun ("p", Fun ("q", App (App (Var "p", Var "p"), Var "q")))
  | Syn.Var "+" -> (* let + m n f x = m f (n f x) *)
      Fun ("m", Fun ("n", Fun ("f", Fun ("x",
        App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
  | Syn.Var "*" ->
      Fun ("m", Fun ("n", Fun ("f", App (Var "m", App (Var "n", Var "f")))))
  | Syn.Var "pred" -> Fun ("n", Fun ("f", Fun ("x",
        App (App (App (Var "n",
          Fun ("g", Fun ("h", App (Var "h", App (Var "g", Var "f"))))),
          Fun ("u", Var "x")),
          Fun ("u", Var "u")))))
  | Syn.Var "-" ->
      let pred = Fun ("n", Fun ("f", Fun ("x",
        App (App (App (Var "n",
          Fun ("g", Fun ("h", App (Var "h", App (Var "g", Var "f"))))),
          Fun ("u", Var "x")),
          Fun ("u", Var "u"))))) in
      Fun ("m", Fun ("n", App (App (Var "n", compile (Var "pred")), Var "m")))
  | Syn.Var s -> Var s
  | Syn.Builtin _ -> raise @@ Error "this builtin is not implemented"
  | Syn.Int n -> Fun ("f", Fun ("x", compile_int n))
  | Syn.List n -> raise @@ Error "lists are not implemented"

let rec show = function
  | Fun (x1, ((Fun (y, Var x2)) as e)) when x1 = x2 ->
      "(λ" ^ x1 ^ "." ^ show e ^ ") (= true)"
  | Fun (x, ((Fun (y1, Var y2)) as e)) when y1 = y2 ->
      "(λ" ^ x ^ "." ^ show e ^ ") (= false or 0)"
  | Fun (f, ((Fun (x, App (Var f2, e2))) as e)) when f = f2 ->
      (try "(λ" ^ f ^ "." ^ show e ^ ") (= "
        ^ (string_of_int @@ show_int f e2) ^ ")" with
        | Error "not a number" ->
            "(λ" ^ f ^ "." ^ show e ^ ")")
  | App (e1, e2) -> "(" ^ show e1 ^ " " ^ show e2 ^ ")"
  | Var s -> s
  | Fun (s, e) -> "(λ" ^ s ^ "." ^ show e ^ ")"

let rec replace n e = function
  | App (e1, e2) -> App (replace n e e1, replace n e e2)
  | Var s when s = n -> e
  | Fun (s, e1) when s <> n -> Fun (s, replace n e e1)
  | e -> e

let rec reduce = function
  | App (e1, e2) -> (match reduce e1 with
    | Fun (s, e3) -> reduce @@ replace s (reduce e2) e3
    | e -> App (e, reduce e2))
  | Fun (s, e1) -> Fun (s, reduce e1)
  | e -> e


