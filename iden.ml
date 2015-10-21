(* iden.ml *)

exception Error of string


let check t =
  let rec checkvar n = function
    | [] -> raise @@ Error ("unknown variable " ^ n)
    | x :: _ when x = n -> ()
    | _ :: xs -> checkvar n xs in

  let rec h names = function
    | Syn.Let (n, v, c) -> h (n :: names) v ; h (n :: names) c
    | Syn.Cond (c, t, e) -> h names c ; h names t ; h names e
    | Syn.Fun (n, c) -> h (n :: names) c
    | Syn.Application (e1, e2) -> h names e1 ; h names e2
    | Syn.Var n -> checkvar n names
    | Syn.Int _ | Syn.Bool _ | Syn.Builtin _ | Syn.List _ -> () in

  h (List.map (fun (x, y) -> x) Builtin.builtin) t

