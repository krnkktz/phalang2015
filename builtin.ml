(* builtin.ml *)

open Syn

exception Error of string

let builtin =
  let i_ = function | Int x -> x | _ -> raise @@ Error "not an int" in
  let i f = Fun ("x", Builtin
    (fun x -> Fun ("y", Builtin
      (fun y -> Int (f (i_ x) (i_ y)))))) in
  let b_ = function | Bool x -> x | _ -> raise @@ Error "not a bool" in
  let b f = Fun ("x", Builtin
    (fun x ->Fun ("y", Builtin
      (fun y -> Bool (f (b_ x) (b_ y)))))) in
  let c f = Fun ("x", Builtin
    (fun x -> Fun ("y", Builtin
      (fun y -> Bool (f (i_ x) (i_ y)))))) in

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
    "null", Fun ("x", Builtin
      (fun x -> Bool (x = List Nil)));
    "head", Fun ("x", Builtin
      (fun x -> match x with
        | List Nil -> raise @@ Error "empty list"
        | List Li (x, _) -> x
        | _ -> raise @@ Error "not a list"));
    "tail", Fun ("x", Builtin
      (fun x -> match x with
        | List Nil -> raise @@ Error "empty list"
        | List Li (_, xs) -> List xs
        | _ -> raise @@ Error "not a list"));
    "nil", List Nil;
    ":", Fun ("x", Builtin
      (fun x -> Fun ("y", Builtin
        (fun y -> match y with
          | List z -> List (Li (x, z))
          | _ -> raise @@ Error "not a list"))));
    "not", Fun ("x", Builtin (fun x -> Bool (not @@ b_ x)));
    "__y_combinator__", Fun ("g", 
      Application (Fun ("x",
        Application (Var "g", Application (Var "x", Var "x"))),
        Fun ("x", Application (Var "g", Application (Var "x", Var "x")))));

  ]

