(* file.ml *)


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s) ;;

let l = Array.length Sys.argv in
let fo = (l > 1 && Sys.argv.(1) = "-d") || (l > 2 && Sys.argv.(2) = "-d") in
let fp =
  if l > 1 && Sys.argv.(1) <>  "-d" then Sys.argv.(1)
  else if l > 2 && Sys.argv.(2) <> "-d" then Sys.argv.(2)
  else "" in
if fp = "" then Top.f () else
  let lex = List.rev @@ Lex.l @@ load_file fp in
  if fo then (
    print_string "lex: " ;
    print_string @@ Lex.show lex ;
    print_newline ()) ;
  let syn = Syn.syn lex in
  if fo then (
    print_string "syn: " ;
    print_string @@ Syn.show syn ;
    print_newline () ;
    print_string "stat: ") ;
  let () = Iden.check syn in
  if fo then (
    print_string "ok" ;
    print_newline () ;
    print_string "eval: ") ;
  (print_string @@ Syn.show @@ Eval.eval Builtin.builtin @@ Syn.syn
    @@ List.rev @@ Lex.l @@ load_file Sys.argv.(1) ;
  print_string "\n") ;;

