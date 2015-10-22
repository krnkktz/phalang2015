(* top.ml *)

let rec f () = (try (
  let () = print_string "phalang15> " in
  let a = read_line () in
  (if (Array.length Sys.argv > 1 && Sys.argv.(1) = "-d") then
    print_string "lex: ") ;
  let lex = List.rev @@ Lex.l a in
  if (Array.length Sys.argv > 1 && Sys.argv.(1) = "-d") then (
    print_string @@ Lex.show lex ;
    print_string "\n" ;
    print_string "syn: ") ;
  let syn = Syn.syn lex in
  if (Array.length Sys.argv > 1 && Sys.argv.(1) = "-d") then (
    print_string @@ Syn.show syn ;
    print_string "\n" ;
    print_string "stat: ") ;
  let () = Iden.check syn in
  if (Array.length Sys.argv > 1 && Sys.argv.(1) = "-d") then (
    print_string "ok." ;
    print_newline () ;
    print_string "eval: ") ;
  let () = print_string @@ Syn.show @@ Eval.eval syn in
  let () = print_string "\n" in ()
  ) with
  | Eval.Error x | Syn.Error x | Lex.Error x | Iden.Error x ->
      print_string ("error: " ^ x ^ "\n")) ;
  f () ;;

