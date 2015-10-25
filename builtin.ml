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
    "nil", List Nil;

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


    ":", Fun ("x", Builtin
      (fun x -> Fun ("y", Builtin
        (fun y -> match y with
          | List z -> List (Li (x, z))
          | _ -> raise @@ Error "not a list"))));

    "not", Fun ("x", Builtin (fun x -> Bool (not @@ b_ x)));

    (* fun f g  -> (fun x a -> f (x x) a) (fun x a -> f (x x) a) g *)
    let sub = (Fun ("_x_", Fun ("_a_",
      Application (
        Application (Var "_f_",
          Application (Var "_x_", Var "_x_")), Var "_a_")))) in

    "__y_combinator__", Fun ("_f_", Fun ("_g_",
      Application (Application (sub, sub), Var "_g_"))) ;
  ]

