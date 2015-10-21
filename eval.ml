(* eval.ml *)

exception Error of string

let rec get_var s = function
  | (n, v) :: _ when n = s -> v
  | _ :: xs -> get_var s xs
  | [] -> raise @@ Error ("unknown variable " ^ s)

let rec eval names = function
  | Syn.Application (exp1, exp2) -> (match eval names exp1 with
    | Syn.Fun f -> f @@ eval names exp2
    | _ -> raise @@ Error "can't apply that!")
  | Syn.Var s -> get_var s names
  | Syn.Cond (c, i, e) -> (match eval names c with
    | Syn.Bool true -> eval names i
    | Syn.Bool false -> eval names e
    | _ -> raise @@ Error "condition is not boolean")
  | Syn.Let (n, v, c) -> eval ((n, eval names v) :: names) c
  | Syn.Int n -> Syn.Int n
  | Syn.Bool b -> Syn.Bool b
  | Syn.Fun t -> Syn.Fun t

let builtin =
  let i_ = function | Syn.Int x -> x | _ -> raise @@ Error "not an int" in
  let i f = Syn.Fun (fun x -> Syn.Fun (fun y ->
    Syn.Int (f (i_ x) (i_ y)))) in
  let b_ = function | Syn.Bool x -> x | _ -> raise @@ Error "not a bool" in
  let b f = Syn.Fun (fun x -> Syn.Fun (fun y ->
    Syn.Bool (f (b_ x) (b_ y)))) in

  [
    "+", i ( + );
    "-", i ( - );
    "*", i ( * );
    "/", i ( / );
    "%", i ( mod );
    "and", b ( && );
    "or", b ( || );
    "not", Syn.Fun (fun x -> Syn.Bool (not @@ b_ x))]

