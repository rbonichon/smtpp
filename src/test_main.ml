let umsg = "Usage: smtpp <file>";;


(*
 * Specification of the known command-line switches of this program.
 * See OCaml's Arg module.
*)
let rec argspec =
  [
  "--help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-pp", Arg.Unit (fun () -> Config.set_reprint true),
  " prints the SMT-LIB AST read on stdout";
  "-debug", Arg.Unit (fun () -> Config.set_debug true),
  " enables debug messages";
  "-multi", Arg.Unit (fun () -> Config.set_pushpop true),
  " generates independent SMTLIB scripts for each (check-sat) command";
  "-disable-success", Arg.Unit (fun () -> Config.set_smtsuccess false),
  " do not print success while parsing";
  "-obfuscate", Arg.Unit (fun () -> Config.set_obfuscate true),
  " generates obfuscated version of SMT script";
  "-keep", Arg.String Config.set_keep_symbols,
  " do not obfuscate this comma separated list of symbols";
  "-version", Arg.Unit Config.pp_version,
  " prints version number";
  "-test-detect-logic", Arg.Unit (fun () -> Config.set_detect true),
  " infer the logic used by SMT-LIB script (alpha)";
  "-undef-unused", Arg.Unit (fun () -> Config.set_unused true),
  " infer the logic used by SMT-LIB script (alpha)";
]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0;
;;

let fname = ref ""
and oc = ref stdout
and fmt = ref Format.std_formatter
and current_file = ref ""
;;

let create_log_file () =
   let test_log_file, f_oc = Filename.open_temp_file "test_smtpp" "pq.log" in
   Io.log "New log file %s created@." test_log_file;
   fname := test_log_file;
   oc := f_oc;
   fmt := Format.formatter_of_out_channel f_oc;
;;

let mk_tests do_test =
  create_log_file ();
  List.iter
    (fun f ->
     let lexbuf, close = Do_parse.lex_file f in
     current_file := f;
     begin
       try
         let script = Parser.script Lexer.token lexbuf in
         let ext_script = Extended_ast.load_theories script in
         do_test ext_script;
       with
       | _ -> Format.fprintf !fmt "%s : ERROR@." !current_file
     end;
     close ();
    ) (Config.get_files ());
  close_out !oc;
;;

let test_detection s =
  let s = Extended_ast.to_ast_script s in
  let detected_logic = Inferlogic.detect_logic s in
  let declared_logic = Logic.parse_logic (Ast_utils.get_logic s) in
  if not (Logic.equal detected_logic declared_logic) then
    Format.fprintf
      !fmt
      "%s : %a (declared), %a (detected)@."
      !current_file
      Logic.pp_from_core declared_logic
      Logic.pp_from_core detected_logic;
;;

let main () =
  Arg.parse argspec Config.add_file umsg;
  if Config.get_detect () then mk_tests test_detection;
;;

main ()

(* Local Variables: *)
(* compile-command: "make tester" *)
(* End: *)
