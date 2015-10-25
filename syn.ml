(* syn.ml *)

exception Error of string

type t =
  | Application of t * t
  | Var of string
  | Cond of t * t * t
  | Int of int
  | Bool of bool
  | Fun of string * t
  | Builtin of (t -> t)
  | List of li

and li =
  | Nil
  | Li of t * li

let rec show = function
  | Application (t1, t2) -> "application (" ^ show t1 ^ ", " ^ show t2 ^ ")"
  | Var s -> "var " ^ s
  | Cond (c, i, e) -> "cond (" ^ show c ^ ", " ^ show i ^ ", " ^ show e ^ ")"
  | Int n -> "int " ^ string_of_int n
  | Bool true -> "bool true"
  | Bool false -> "bool false"
  | Fun (n, x) -> "fun " ^ n ^ " -> " ^ show x
  | Builtin _ -> "builtin"
  | List Nil -> "nil"
  | List Li (x, xs) -> show x ^ " : " ^ show @@ List xs

let syn l t =

  let rec gen_f r args exp = match args with
    | [] -> exp
    | x :: xs -> Fun (x, gen_f r xs exp) in

  let rec args_h acc = function
    | Lex.Id name :: xs -> args_h (name :: acc) xs
    | xs -> acc, xs in

  let rec subexpr = function
    | Lex.If :: xs -> let ex1, xs1 = expr xs in (match xs1 with
      | Lex.Then :: xs2 ->
        let ex2, xs3 = expr xs2 in (match xs3 with
          | Lex.Else :: xs4 -> let ex3, xs5 = expr xs4 in
            Cond (ex1, ex2, ex3), xs5
          | _ -> raise @@ Error "expected else")
        | _ -> raise @@ Error "expected then")
    | Lex.Let :: Lex.Id name :: xs ->
      f_let name false xs
    | Lex.Let :: Lex.Rec :: Lex.Id name :: xs ->
      f_let name true xs
    | Lex.Let :: _ -> raise @@ Error "weird let"
    | Lex.Fun :: Lex.Id name :: Lex.To :: xs ->
      let exp, xs1 = expr xs in
        Fun (name, exp), xs1
    | Lex.Fun :: _ -> raise @@ Error "weird fun"
    | Lex.Int x :: xs -> Int x, xs
    | Lex.Bool x :: xs -> Bool x, xs
    | Lex.Id "lib" :: xs -> l, xs
    | Lex.Id x :: xs -> Var x, xs
    | Lex.Leftpar :: xs -> let ex, xxs = expr xs in (match xxs with
      | Lex.Rightpar :: xxxs -> ex, xxxs
      | _ -> raise @@ Error "missing right parenthesis")
    | xs -> raise @@ Error "expected a subexpr" and

  f_let name r xs =
    match args_h [] xs with
      | args, Lex.Assign :: xs ->
        let exp, xs1 = expr xs in (match xs1 with
          | Lex.In :: xs2 -> let exp1, xs3 = expr xs2 in
            if r then
              Application (Fun ("rec",
                Application (Fun (name, exp1),
                  Application (Var "__y_combinator__", Var "rec"))),
                Fun (name, gen_f r (List.rev args) exp)), xs3
            else
              (Application (Fun (name, exp1),
                gen_f r (List.rev args) exp), xs3)
          | _ -> raise @@ Error "weird let, expected in")
      | _ -> raise @@ Error "weird let, expected =" and

  s_fun xs =
    let rec h a xs = try (match subexpr xs with
        | e, [] -> Application (a, e), []
        | e, xs2 -> h (Application (a, e)) xs2) with
      | Error "expected a subexpr" -> a, xs in

    match subexpr xs with
      | e, [] -> e, []
      | e, xs1 -> h e xs1 and

  s_times xs =
    let rec h = function
      | e, Lex.Operator Lex.Times :: xs -> let e1, xs1 = s_times xs in
        Application (Application (Var "*", e), e1), xs1
      | e, Lex.Operator Lex.Division :: xs -> let e1, xs1 = s_times xs in
        Application (Application (Var "/", e), e1), xs1
      | e, Lex.Operator Lex.Modulo :: xs -> let e1, xs1 = s_times xs in
        Application (Application (Var "%", e), e1), xs1
      | e, xs -> e, xs in
    h @@ s_fun xs and

  s_plus xs =
    let rec h = function
      | e, Lex.Operator Lex.Plus :: xs -> let e1, xs1 = s_times xs in
        h (Application (Application (Var "+", e), e1), xs1)
      | e, Lex.Operator Lex.Minus :: xs -> let e1, xs1 = s_times xs in
        h (Application (Application (Var "-", e), e1), xs1)
      | e, xs -> e, xs in
    h @@ s_times xs and

  s_cons xs = match s_plus xs with
    | e, Lex.Operator Lex.Cons :: xs -> let e1, xs2 = s_cons xs in
      Application (Application (Var ":", e), e1), xs2
    | e, xs -> e, xs and

  s_comp xs =
    let rec h = function
      | e, Lex.Operator Lex.Equal :: xs -> let e1, xs1 = s_comp xs in
        Application (Application (Var "==", e), e1), xs1
      | e, Lex.Operator Lex.Greater :: xs -> let e1, xs1 = s_comp xs in
        Application (Application (Var ">", e), e1), xs1
      | e, Lex.Operator Lex.Lesser :: xs -> let e1, xs1 = s_comp xs in
        Application (Application (Var "<", e), e1), xs1
      | e, Lex.Operator Lex.Greaterorequal :: xs -> let e1, xs1 = s_comp xs in
        Application (Application (Var ">=", e), e1), xs1
      | e, Lex.Operator Lex.Lesserorequal :: xs -> let e1, xs1 = s_comp xs in
        Application (Application (Var "<=", e), e1), xs1
      | e, xs -> e, xs in
    h @@ s_cons xs and

  s_bool xs = match s_comp xs with
    | e, Lex.Operator Lex.And :: xs1 -> let e1, xs2 = s_bool xs1 in
      Application (Application (Var "and", e), e1), xs2
    | e, Lex.Operator Lex.Or :: xs1 -> let e1, xs2 = s_bool xs1 in
      Application (Application (Var "or", e), e1), xs2
    | e, xs -> e, xs and

  expr xs = s_bool xs in

  match expr t with
    | e, [] -> e
    | e, xs -> raise @@ Error "how is that even possible"

