(* file.ml *)


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s) ;;

if Array.length Sys.argv > 1 then
  let lex = List.rev @@ Lex.l @@ load_file Sys.argv.(1) in
  let () = print_string "lex: ok" in
  let () = print_newline () in
  let syn = Syn.syn lex in
  let () = print_string "syn: ok" in
  let () = print_newline () in
  let () = print_string "stat: " in
  let () = Iden.check syn in
  let () = print_string "ok" in
  let () = print_newline () in
  let () = print_string "eval: " in
  (print_string @@ Syn.show @@ Eval.eval Builtin.builtin @@ Syn.syn
    @@ List.rev @@ Lex.l @@ load_file Sys.argv.(1) ;
  print_string "\n")
else Top.f () ;;

