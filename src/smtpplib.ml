(* Extra file to define accessible elements from C code *)
let _ =
  Callback.register "parse" Do_parse.apply;
;;
