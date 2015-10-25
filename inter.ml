(* inter.ml *)

let inter f_show f_eval use_libs =
  let libdebug = false in

  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = String.create n in
    really_input ic s 0 n;
  close_in ic;
  (s) in

  let makesyn l d s =
    if d then (print_string "lex: ") ;
    let lex = List.rev @@ Lex.l s in
    if d then (print_string @@ Lex.show lex ; print_newline ()) ;
    if d then (print_string "syn: ") ;
    let syn = Syn.syn l lex in
    if d then (print_string @@ Syn.show true syn; print_newline ()) ;
    syn in

  let exec d s =
    if (d && libdebug) then (print_string "exp : " ;
    print_newline () ;
    print_string "-----" ;
    print_newline ());
    let e = makesyn (Syn.List Syn.Nil) d s in
    let e2 = if use_libs then (
      if d && libdebug then (print_newline () ;
        print_string "lib : " ;
        print_newline () ;
        print_string "-----" ;
        print_newline ()) ;
      makesyn (e) (d && libdebug) @@ load_file "pg/lib.pg") else e in
    if d then (print_string "stat: ") ;
    Iden.check e2 ;
    if d then (print_string "ok" ; print_newline () ; print_string "eval : ") ;
    f_eval e2 in


  let rec top d = (try (
    let () = print_string "phalang15> " in
    let a = read_line () in
    print_string @@ f_show @@ exec d a ;
    print_newline ()) with
    | Eval.Error x | Syn.Error x | Lex.Error x
    | Iden.Error x | Builtin.Error x ->
      (print_string ("error: " ^ x) ; print_newline ())) ;
    top d in


  let l = Array.length Sys.argv in
  let d = (l > 1 && Sys.argv.(1) = "-d") || (l > 2 && Sys.argv.(2) = "-d") in
  let fp =
    if l > 1 && Sys.argv.(1) <>  "-d" then Sys.argv.(1)
    else if l > 2 && Sys.argv.(2) <> "-d" then Sys.argv.(2)
    else "" in
  if fp = "" then top d else
    (if d then (fun x -> print_string @@ f_show x ; print_newline ())
    else (fun _ -> ())) @@ exec d @@ fp ;;

