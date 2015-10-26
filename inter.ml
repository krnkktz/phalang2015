(* inter.ml *)

let inter f_print f_newline f_show f_eval use_libs web s =
  let libdebug = false in

  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = String.create n in
    really_input ic s 0 n;
  close_in ic;
  (s) in

  let makesyn l d s =
    if d then (f_print "lex: ") ;
    let lex = List.rev @@ Lex.l s in
    if d then (f_print @@ Lex.show lex ; f_newline ()) ;
    if d then (f_print "syn: ") ;
    let syn = Syn.syn l lex in
    if d then (f_print @@ Syn.show true syn; f_newline ()) ;
    syn in

  let exec d s =
    if (d && libdebug) then (f_print "exp : " ;
    f_newline () ;
    f_print "-----" ;
    f_newline ());
    let e = makesyn (Syn.List Syn.Nil) d s in
    let e2 = if use_libs then (
      if d && libdebug then (f_newline () ;
        f_print "lib : " ;
        f_newline () ;
        f_print "-----" ;
        f_newline ()) ;
      makesyn (e) (d && libdebug) @@ load_file "pg/lib.pg") else e in
    if d then (f_print "stat: ") ;
    Iden.check e2 ;
    if d then (f_print "ok" ; f_newline () ; f_print "eval : ") ;
    f_eval e2 in


  let rec top d = (try (
    (if not web then f_print "phalang15> ") ;
    let a = (if not web then read_line () else s) in
    f_print @@ f_show @@ exec d a ;
    f_newline ()) with
    | Eval.Error x | Syn.Error x | Lex.Error x
    | Iden.Error x | Builtin.Error x ->
      (f_print ("error: " ^ x) ; f_newline ())) ;
    if (not web) then top d in


  if web then top false else
  let l = Array.length Sys.argv in
  let d = (l > 1 && Sys.argv.(1) = "-d") || (l > 2 && Sys.argv.(2) = "-d") in
  let fp =
    if l > 1 && Sys.argv.(1) <>  "-d" then Sys.argv.(1)
    else if l > 2 && Sys.argv.(2) <> "-d" then Sys.argv.(2)
    else "" in
  if fp = "" then top d else
    (if d then (fun x -> f_print @@ f_show x ; f_newline ())
    else (fun _ -> ())) @@ exec d @@ fp ;;

