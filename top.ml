(* top.ml *)

let rec f () = (try (
  print_string "phalang15> " ;
  let a = read_line () in
  print_string "lex: ";
  let lex = List.rev @@ Lex.l a in
  print_string @@ Lex.show lex ; print_string "\n" ;
  print_string "syn: " ;
  let syn = Syn.syn lex in
  print_string @@ Syn.show syn ; print_string "\n" ;
  print_string "eval: " ;
  print_string @@ Show.show @@ Eval.eval Eval.builtin syn ;
  print_string "\n" ;
  ) with
  | Eval.Error x | Syn.Error x | Lex.Error x ->
      print_string ("error: " ^ x ^ "\n")) ;
  f () ;;

