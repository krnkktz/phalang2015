(* eval.ml *)

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
  | Syn.Application (exp1, exp2) -> (match eval names exp1 with
    | Syn.Fun (_, Syn.Builtin f) -> f @@ eval names exp2
    | Syn.Fun (n, f) -> let v = eval names exp2 in
      eval ((n, v) :: names) @@ closure n v f
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
  | Syn.List x -> Syn.List x and

  closure n v = function
    | Syn.Application (e1, e2) ->
        Syn.Application (closure n v e1, closure n v  e2)
    | Syn.Var s when s = n -> v 
    | Syn.Cond (c, i, e) ->
      Syn.Cond (closure n v c, closure n v i, closure n v e)
    | Syn.Let (n1, v1, c) when n1 = n -> Syn.Let (n1, closure n v v1, c)
    | Syn.Let (n1, v1, c) -> Syn.Let (n1, closure n v v1, closure n v c)
    | Syn.Fun (n1, e) when n1 = n -> Syn.Fun (n1, e)
    | Syn.Fun (n1, e) -> Syn.Fun (n1, closure n v e)
    | e -> e


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
    "head", Syn.Fun ("x", Syn.Builtin
      (fun x -> match x with
        | Syn.List Syn.Nil -> raise @@ Error "empty list"
        | Syn.List Syn.Li (x, _) -> x
        | _ -> raise @@ Error "not a list"));
    "tail", Syn.Fun ("x", Syn.Builtin
      (fun x -> match x with
        | Syn.List Syn.Nil -> raise @@ Error "empty list"
        | Syn.List Syn.Li (_, xs) -> Syn.List xs
        | _ -> raise @@ Error "not a list"));
    "nil", Syn.List Syn.Nil;
    ":", Syn.Fun ("x", Syn.Builtin
      (fun x -> Syn.Fun ("y", Syn.Builtin
        (fun y -> match y with
          | Syn.List z -> Syn.List (Syn.Li (x, z))
          | _ -> raise @@ Error "not a list"))));
    "not", Syn.Fun ("x", Syn.Builtin (fun x -> Syn.Bool (not @@ b_ x)))]

