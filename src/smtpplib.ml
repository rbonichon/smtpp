(* Simple example callback to call from C code *)

let parse_file filename =
  Config.set_file filename;
  Do_parse.apply ();
;;

(* Extra file to define accessible elements from C code *)
let _ =
  Callback.register "parse_file" parse_file;
;;
