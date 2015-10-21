(* top.ml *)

let rec f () =
  print_string "phalang15> " ;
  let a = read_line () in
  let syn = Syn.syn @@ List.rev @@ Lex.l a in
  print_string "syntax: " ;
  print_string @@ Syn.show syn ; print_string "\n" ;
  print_string @@ Show.show @@ Eval.eval Eval.builtin syn ;
  print_string "\n" ;
  f () ;;

f () ;;

