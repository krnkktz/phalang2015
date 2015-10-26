(* run.ml *)

open Ulambda ;;

Inter.inter print_string print_newline show
  (fun x -> let y = compile x in reduce y) false false "" ;;

