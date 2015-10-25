(* run.ml *)

let libdebug = false ;;

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s) ;;

let makesyn l d s =
  if d then (print_string "lex: ") ;
  let lex = List.rev @@ Lex.l s in
  if d then (print_string @@ Lex.show lex ; print_newline ()) ;
  if d then (print_string "syn: ") ;
  let syn = Syn.syn l lex in
  if d then (print_string @@ Syn.show syn; print_newline ()) ;
  syn ;;

let exec d s =
  if (d && libdebug) then (print_string "exp : " ;
  print_newline () ;
  print_string "-----" ;
  print_newline ());
  let e = makesyn (Syn.List Syn.Nil) d s in
  if d && libdebug then (print_newline () ;
  print_string "lib : " ;
  print_newline () ;
  print_string "-----" ;
  print_newline ()) ;
  let ns = makesyn (e) (d && libdebug) @@ load_file "pg/lib.pg" in
  if d then (print_string "stat: " ; Iden.check ns ;
  print_string "ok" ; print_newline () ; print_string "eval : ") ;
  Eval.eval ns


let rec top d = (try (
  let () = print_string "phalang15> " in
  let a = read_line () in
  print_string @@ Syn.show @@ exec d a ;
  print_newline ()) with
  | Eval.Error x | Syn.Error x | Lex.Error x | Iden.Error x ->
      (print_string ("error: " ^ x) ; print_newline ())) ; top d ;;


let l = Array.length Sys.argv in
let d = (l > 1 && Sys.argv.(1) = "-d") || (l > 2 && Sys.argv.(2) = "-d") in
let fp =
  if l > 1 && Sys.argv.(1) <>  "-d" then Sys.argv.(1)
  else if l > 2 && Sys.argv.(2) <> "-d" then Sys.argv.(2)
  else "" in
if fp = "" then top d else
(if d then (fun x -> print_string @@ Syn.show x ; print_newline ())
else (fun _ -> ())) @@
  exec d @@ fp ;;
