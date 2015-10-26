(* web.ml *)

let r = ref "" ;;

let add_string s =
  r := !r ^ s ;;

let add_newline () =
  r := !r ^ "<br />" ;;

let run ul s =
  let () =
    (if Js.to_bool ul then
      Inter.inter add_string add_newline Ulambda.show
      (fun x -> let y = Ulambda.compile x in Ulambda.reduce y)
      false true @@ Js.to_string s
    else
      Inter.inter add_string add_newline
      (Syn.show false) Eval.eval false true @@ Js.to_string s) in

  let res = !r in r := "" ; Js.string @@ res ;;

Js.Unsafe.global##run <- Js.wrap_callback run ;;

