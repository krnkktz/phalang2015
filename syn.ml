(* syn.ml *)

exception Error of string

type t =
  | Application of t * t
  | Var of string
  | Cond of t * t * t
  | Let of string * t * t
  | Int of int
  | Bool of bool
  | Fun of (t -> t)

let rec show = function
  | Application (t1, t2) -> "Application (" ^ show t1 ^ ", " ^ show t2 ^ ")"
  | Var s -> "Var " ^ s
  | Cond (c, i, e) -> "Cond (" ^ show c ^ ", " ^ show i ^ ", " ^ show e ^ ")"
  | Let (n, v, c) -> "Let (" ^ n ^ ", " ^ show v ^ ", " ^ show c ^ ")"
  | Int n -> "Int " ^ string_of_int n
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | Fun _ -> "Fun"

let syn t =

  let var_of_op = function
    | Lex.Times -> "*"
    | Lex.Division  -> "/"
    | Lex.Modulo -> "%"
    | Lex.Plus -> "+"
    | Lex.Minus  -> "-"
    | Lex.Equal  -> "=="
    | Lex.Greater  -> ">"
    | Lex.Lesser  -> "<"
    | Lex.Greaterorequal  -> ">="
    | Lex.Lesserorequal  -> "<="
    | Lex.And  -> "and"
    | Lex.Or  -> "or"
    | Lex.Not -> "not"
  in

  let rec tr = function
    | Lex.If :: xs -> let ex1, xs1 = tr_super xs in (match xs1 with
      | Lex.Then :: xs2 -> let ex2, xs3 = tr_super xs2 in (match xs3 with
        | Lex.Else :: xs4 -> let ex3, xs5 = tr_super xs4 in
          Cond (ex1, ex2, ex3), xs5
        | _ -> raise @@ Error "expected else")
      | _ -> raise @@ Error "expected then")
    | Lex.Let :: Lex.Id name :: Lex.Assign :: xs ->
      let exp, xs1 = tr_super xs in (match xs1 with
        | Lex.In :: xs2 -> let exp1, xs3 = tr_super xs2 in
          Let (name, exp, exp1), xs3
        | _ -> raise @@ Error "expected in")
    | Lex.Let :: _ -> raise @@ Error "weird let"
    | Lex.Int x :: xs -> Int x, xs
    | Lex.Bool x :: xs -> Bool x, xs
    | Lex.Id x :: xs -> Var x, xs
    | Lex.Operator Lex.Not :: xs -> let exp, xs1 = tr_super xs in
      Application (Var "not", exp), xs1
    | Lex.Leftpar :: xs -> let ex, xxs = tr_super xs in (match xxs with
      | Lex.Rightpar :: xxxs -> ex, xxxs
      | _ -> raise @@ Error "missing right parenthesis")
    | xs -> raise @@ Error "expected an expr" and

  tr_super xs = let exp, xs1 = tr xs in match xs1 with
    | [] -> exp, []
    | Lex.Operator o :: xs1 -> (try let exp1, xs2 = tr xs1 in
        Application (Application (Var (var_of_op o), exp), exp1), xs2 with
        | Error "expected an expr" ->
            Application (Var (var_of_op o), exp), xs1)
    | xs2 -> try (let exp1, xs3 = tr xs2 in Application (exp, exp1), xs3) with
      | Error "expected an expr" -> exp, xs2 in

  match tr_super t with
    | e, [] -> e
    | e, xs -> raise @@Error "weird stuff in the end"


