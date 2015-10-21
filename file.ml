(* file.ml *)


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s) ;;

if Array.length Sys.argv > 1 then
  (print_string @@ Show.show @@ Eval.eval Eval.builtin @@ Syn.syn
    @@ List.rev @@ Lex.l @@ load_file Sys.argv.(1) ;
  print_string "\n")
else Top.f () ;;

