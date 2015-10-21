(* lex.ml *)

exception Error of string

type operator =
  | Greater
  | Lesser
  | Equal
  | Greaterorequal
  | Lesserorequal
  | Plus
  | Minus
  | Times
  | Division
  | Modulo
  | Not
  | And
  | Or

type t =
  | Id of string
  | Int of int
  | Bool of bool
  | Operator of operator
  | Leftpar
  | Rightpar
  | Let
  | Assign
  | In
  | If
  | Then
  | Else
  | Fun
  | To
  | Rec

let rec show = function
  | [] -> ""
  | x :: xs -> (match x with
    | Id s -> s
    | Int n -> string_of_int n
    | Bool true -> "true"
    | Bool false -> "false"
    | Operator _ -> "@"
    | Leftpar -> "("
    | Rightpar -> ")"
    | Let -> "let"
    | Assign -> "="
    | In -> "in"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | Fun -> "fun"
    | Rec -> "rec"
    | To -> "to") ^ " " ^ show xs


let l s =

  let len = String.length s in

  let matches s c =
    let len = String.length s in
    let rec tr pos =
      if pos >= len then
        false
      else if c = s.[pos] then
        true
      else tr (succ pos) in tr 0 in

  let is_varname = matches
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'" in

  let is_num = matches "1234567890" in

  let rec tr acc pos = if pos >= len then acc else
    (match s.[pos] with
      | ' ' | '\t' | '\r' | '\n' -> tr acc
      | '+' -> tr (Operator Plus :: acc)
      | '-' -> tr_comm acc 1 false
      | '*' -> tr (Operator Times :: acc)
      | '/' -> tr (Operator Division :: acc)
      | '%' -> tr (Operator Modulo :: acc)
      | '(' -> tr (Leftpar :: acc)
      | ')' -> tr (Rightpar :: acc)
      | '=' -> tr_eq acc
      | '>' -> tr_gr acc
      | '<' -> tr_lr acc
      | n when is_num n -> tr_num acc @@ int_of_string @@ String.make 1 n
      | n when is_varname n -> tr_var acc @@ String.make 1 n
      | c -> raise @@ Error ("unknown char '" ^ String.make 1 c ^ "'"))
    @@ succ pos and

  tr_eq acc pos =
    if pos < len && s.[pos] = '=' then
      tr (Operator Equal :: acc) (succ pos)
    else
      tr (Assign :: acc) pos and

  tr_gr acc pos =
    if pos < len && s.[pos] = '=' then
      tr (Operator Greaterorequal :: acc) (succ pos)
    else
      tr (Operator Greater :: acc) pos and

  tr_lr acc pos =
    if pos < len && s.[pos] = '=' then
      tr (Operator Lesserorequal :: acc) (succ pos)
    else
      tr (Operator Lesser :: acc) pos and

  tr_var acc cw pos =
    if pos < len && is_varname s.[pos] then
      tr_var acc (cw ^ String.make 1 s.[pos]) @@ succ pos
    else (match cw with
      | "not" -> tr (Operator Not :: acc)
      | "and" -> tr (Operator And :: acc)
      | "or" -> tr (Operator Or :: acc)
      | "true" -> tr (Bool true :: acc)
      | "false" -> tr (Bool false :: acc)
      | "let" -> tr (Let :: acc)
      | "in" -> tr (In :: acc)
      | "if" -> tr (If :: acc)
      | "then" -> tr (Then :: acc)
      | "else" -> tr (Else :: acc)
      | "fun" -> tr (Fun :: acc)
      | "rec" -> tr (Rec :: acc)
      | n -> tr (Id n :: acc)) pos and

  tr_num acc cn pos =
    if pos < len && is_num s.[pos] then
      tr_num acc
      (cn * 10 + (int_of_string @@ String.make 1 s.[pos])) (succ pos)
    else
      tr (Int cn :: acc) pos and

  tr_comm acc nb is pos =
    if pos >= len then acc else
      (if s.[pos] = '>' && nb = 1 && not is then tr (To :: acc) else
        if s.[pos] = '-' then
          if is && nb > 0 then tr acc else tr_comm acc 1 true
      else
        if is then tr_comm acc 0 true else tr (Operator Minus :: acc))
    @@ succ pos in

  tr [] 0 ;;



