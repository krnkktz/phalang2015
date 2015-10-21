(* builtin.ml *)

exception Error of string

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
    "null", Syn.Fun ("x", Syn.Builtin
      (fun x -> Syn.Bool (x = Syn.List Syn.Nil)));
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

