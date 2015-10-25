(* run.ml *)

open Ulambda ;;

Inter.inter show (fun x -> let y = compile x in reduce y) false ;;

