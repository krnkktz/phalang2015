(* eval.ml *)

exception Error of string

let rec get_var s = function
  | (n, v) :: _ when n = s -> v
  | _ :: xs -> get_var s xs
  | [] -> raise @@ Error ("unknown variable " ^ s)

let rec show_namespace = function
  | [] ->  ""
  | (n, v) :: xs -> "var " ^ n ^ ": " ^ Syn.show v ^ "\n" ^ show_namespace xs

let rec eval names = function
  | Syn.Application (exp1, exp2) -> (match eval names exp1 with
    | Syn.Fun (_, Syn.Builtin f) -> f @@ eval names exp2
    | Syn.Fun (n, f) -> let v = eval names exp2 in
      (match eval ((n, v) :: names) f with
        | Syn.Fun (n1, f1) -> Syn.Fun (n1, Syn.Let (n, v, f1))
        | e -> e)
    | _ -> raise @@ Error "can't apply that!")
  | Syn.Var s ->
        get_var s names
  | Syn.Cond (c, i, e) -> (match eval names c with
    | Syn.Bool true -> eval names i
    | Syn.Bool false -> eval names e
    | _ -> raise @@ Error "condition is not boolean")
  | Syn.Let (n, v, c) -> eval ((n, eval names v) :: names) c
  | Syn.Int n -> Syn.Int n
  | Syn.Bool b -> Syn.Bool b
  | Syn.Fun (n, e) -> Syn.Fun (n, e)
  | Syn.Builtin f -> Syn.Builtin f

let builtin =
  let i_ = function | Syn.Int x -> x | _ -> raise @@ Error "not an int" in
  let i f = Syn.Fun ("x", Syn.Builtin
    (fun x -> Syn.Fun ("y", Syn.Builtin
      (fun y -> Syn.Int (f (i_ x) (i_ y)))))) in
  let b_ = function | Syn.Bool x -> x | _ -> raise @@ Error "not a bool" in
  let b f = Syn.Fun ("x", Syn.Builtin
    (fun x ->Syn.Fun ("y", Syn.Builtin
      (fun y -> Syn.Bool (f (b_ x) (b_ y)))))) in
  let c f = Syn.Fun ("x", Syn.Builtin
    (fun x -> Syn.Fun ("y", Syn.Builtin
      (fun y -> Syn.Bool (f (i_ x) (i_ y)))))) in

  [
    "+", i ( + );
    "-", i ( - );
    "*", i ( * );
    "/", i ( / );
    "%", i ( mod );
    "==", c ( == );
    ">", c ( > );
    "<", c ( < );
    ">=", c ( >= );
    "<=", c ( <= );
    "and", b ( && );
    "or", b ( || );
    "not", Syn.Fun ("x", Syn.Builtin (fun x -> Syn.Bool (not @@ b_ x)))]

