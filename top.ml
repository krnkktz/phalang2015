(* top.ml *)

let rec f () = (try (
  let () = print_string "phalang15> " in
  let a = read_line () in
  let () = print_string "lex: " in
  let lex = List.rev @@ Lex.l a in
  let () = print_string @@ Lex.show lex in
  let () = print_string "\n"  in
  let () = print_string "syn: "  in
  let syn = Syn.syn lex in
  let () = print_string @@ Syn.show syn in
  let () = print_string "\n" in
  let () = print_string "eval: " in
  let () = print_string @@ Show.show @@ Eval.eval Eval.builtin syn in
  let () = print_string "\n" in ()
  ) with
  | Eval.Error x | Syn.Error x | Lex.Error x ->
      print_string ("error: " ^ x ^ "\n")) ;
  f () ;;

