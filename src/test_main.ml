open Utils ;;

(* Some specific configuration variables *)
let smt_directory = ref "" ;;
let output_directory = ref "." ;;

type mode = SMTLIB | Simple ;;

let test_mode = ref Simple ;;

let umsg = "Usage: smtpp_tester <file>";;

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
  "-obfuscate", Arg.Unit (fun () -> Config.set_obfuscate true),
  " generates obfuscated version of SMT script";
  "-keep", Arg.String Config.set_keep_symbols,
  " do not obfuscate this comma separated list of symbols";
  "-version", Arg.Unit Config.pp_version,
  " prints version number";
  "-test-detect-logic", Arg.Unit (fun () -> Config.set_detect true),
  " generate test for detect logic";
  "-test-undef-unused", Arg.Unit (fun () -> Config.set_unused true),
  " generate test for undef/unused";
  "-smtdir", Arg.String (fun s -> smt_directory := s; test_mode := SMTLIB),
  " set and use this directory as a base for tests. Each subdir is assumed to \
   be a SMT category";
  "-outdir", Arg.Set_string output_directory,
  " uses this directory for logging/output (defaults to .)";
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

let chop_path_prefix path1 path2 =
  let rec aux basenames dirname =
    Io.debug "%s@." dirname;
    if String.compare dirname path1 = 0
       || String.compare dirname "." = 0
    then
      begin
      Io.debug "Chop out@.";
      match basenames with
      | [] ->  ""
      | dir :: dirs ->
         List.fold_left (fun p n -> Filename.concat p n) dir dirs
      end
    else aux (Filename.basename dirname :: basenames) (Filename.dirname dirname)
  in aux [] path2
;;

let create_log_file testname dir () =
  let prefix, suffix =
    match !test_mode with
    | SMTLIB -> testname, Filename.basename dir
    | Simple -> "_generic", ""
  in
   let test_log_file, f_oc =
     Filename.open_temp_file ("smtpp_"^prefix^"_"^suffix) (".md") in
   Io.log "New log file %s created@." test_log_file;
   fname := test_log_file;
   oc := f_oc;
   fmt := Format.formatter_of_out_channel f_oc;
;;

let close_log () =
  Io.log "Closing %s@." !fname;
  Pervasives.close_out !oc;
  fname := "";
  oc := stdout ;
  fmt := Format.std_formatter
;;

let pp_time fmt (tm : Unix.tm) =
  let open Unix in
  Format.fprintf fmt "%d-%d-%d %d:%d"
                 tm.tm_year tm.tm_mon tm.tm_mday tm.tm_hour tm.tm_min
;;

let exn_to_string e =
  match e with
  | Stack_overflow -> "stack overflow"
  | Parser.Error -> "parse error"
  | Lexer.LexError _ -> "lex error"
  | _ -> "unknown error"
;;

let mk_tests testname dir pre_tests do_test post_tests =
  create_log_file testname dir ();
  let basedir = Filename.basename dir in
  Format.fprintf !fmt "## %s@." basedir;
  pre_tests ();
  let errors = ref 0 in
  let time = Unix.gmtime (Unix.time ()) in
  Format.fprintf !fmt "@[<v 0>@ \
                       ~~~@ BEGIN %s %a@ " testname pp_time time;
  List.iter
    (fun f ->
     Io.log "%s@." f;
     let lexbuf, close = Do_parse.lex_file f in
     current_file := f;
     begin
       try
         let script = Parser.script Lexer.token lexbuf in
         let ext_script = Extended_ast.load_theories script in
         do_test ext_script;
       with
       | e ->
          (Format.fprintf
             !fmt "@[<v 4>%s@ ERROR (%s)@]@ "
             (chop_path_prefix !smt_directory !current_file)
             (exn_to_string e) ;
           Io.log "error@.";
            incr errors ;)
     end;
     close ();
    ) (Config.get_files ());
  Format.fprintf !fmt "END@ ~~~@]@.@.";
  post_tests ();
  Format.fprintf !fmt "* **Errors** : %d@.@." !errors;
  Format.pp_print_flush !fmt ();
  close_log ()
;;

let init_test_detection, test_detection , end_test_detection =
  let h = Hashtbl.create 7 in
  let ntests = ref 0 in
  let alerts = ref 0 in
  let over = ref 0 in
  let under = ref 0 in
  let both = ref 0 in
  (fun () ->
   Hashtbl.reset h;
   ntests := 0;
   over := 0;
   under := 0;
   both := 0;
   alerts := 0;
   Format.fprintf !fmt "### Raw results@.@."
  ),
  (fun s ->
   incr ntests;
   let s = Extended_ast.to_ast_script s in
   let detected_logic = Inferlogic.detect_logic s in
   let declared_logic = Logic.parse_logic (Ast_utils.get_logic s) in
   if not (Logic.equal detected_logic declared_logic) then
     begin
       Io.debug "Diff detected@.";
       incr alerts;
       Io.debug "Printing@.";
       Format.fprintf
         !fmt
         "@[<v 4>%d. %s@ %a (declared), %a (detected)@]@ "
         !alerts
         (chop_path_prefix !smt_directory !current_file)
         Logic.pp_from_core declared_logic
         Logic.pp_from_core detected_logic;
       Io.debug "Printed msg@.";
       (try
         let v = Hashtbl.find h detected_logic in
         Hashtbl.replace h detected_logic (succ v)
         with Not_found -> Hashtbl.add h detected_logic 1);
       Io.debug "Registerd htbl@.";
       (match Logic.one_bigger_dimension detected_logic declared_logic,
              Logic.one_bigger_dimension declared_logic detected_logic
        with
        | true, true -> incr both
        | true, false -> incr over
        | false, true -> incr under
        | false, false -> assert false);
     end
  ),
  (fun () ->
   Format.fprintf
     !fmt
     "@[<v 0>\
      ### Summary of detection@ \
      * Alerts : %d / %d@ \
      * Over : %d@ \
      * Under : %d@ \
      * OverUnder : %d@ \
      * @[<v 4>**By categories** :@ %a@] \
      @]@.\
      "
     !alerts !ntests !over !under !both
     (fun fmt h ->
      Hashtbl.iter
        (fun k v -> Format.fprintf fmt "* %a : %d@ " Logic.pp_from_core k v)
        h
     ) h;
  )
;;



let init_test_use_def, test_use_def, end_test_use_def =
  let alerts = ref 0 in
  let ntests = ref 0 in
  let nunused = ref 0 in
  let nundef = ref 0 in
  (fun () ->
   ntests := 0;
   alerts := 0;
   nundef := 0;
   nunused := 0;
   Format.fprintf !fmt "### Raw results@.@."
  ),
  (fun s ->
   incr ntests;
   let (unused, undef) as uures = Undef_unused.apply s in
   let has_unused = not (SymbolSet.is_empty unused)
   and has_undef = not (SymbolSet.is_empty undef)
   in
   if has_unused || has_undef then begin
     incr alerts;
     Format.fprintf
       !fmt "%d. @[<v 0>%s@ @ %a@]@."
       !alerts
       (chop_path_prefix !smt_directory !current_file)
       Undef_unused.pp_uu uures
     end;
   if has_unused then incr nunused ;
   if has_undef then incr nundef;
  ),
  (fun () ->
   Format.fprintf
     !fmt
     "@[<v 0>\
      ### Summary of detection@ \
      * Alerts : %d / %d@ \
      * With unused : %d@ \
      * With undefined : %d@ \
      @]@.\
      "
     !alerts !ntests !nunused !nundef
  )
;;

let execute_tests_on_files ?dir:(ldir="") () =
  if Config.get_detect () then
    mk_tests "logic_inf" ldir
             init_test_detection test_detection end_test_detection;
  if Config.get_unused () then
    mk_tests "def_use" ldir init_test_use_def test_use_def end_test_use_def;
;;

let list_directories dir =
  List.sort
    String.compare (
      List.filter
        Sys.is_directory
        (List.map
           (Filename.concat dir)(Array.to_list (Sys.readdir dir))))
;;

let list_smt2_files dir =
  let is_smt2file filename = Filename.check_suffix filename ".smt2" in
  let rec aux dirs files =
    match dirs with
    | [] -> files
    | dir :: dirs ->
       Io.debug "Listing %s@." dir;
       let dirfiles = List.map (Filename.concat dir)
                            (Array.to_list (Sys.readdir dir)) in
       let subdirs, pure_files = List.partition Sys.is_directory dirfiles in
       let smt2files = List.filter is_smt2file pure_files in
       aux (dirs @ subdirs) (smt2files @ files)
  in aux [dir] []
;;

let main () =
  Arg.parse argspec Config.add_file umsg;
  if !output_directory <> "" then begin
      (if not (Sys.file_exists !output_directory)
       then Unix.mkdir !output_directory 0o700
      else if not (Sys.is_directory !output_directory) then begin
          Sys.remove !output_directory;
          Unix.mkdir !output_directory 0o700;
        end);
      Filename.set_temp_dir_name !output_directory;
    end;

  if !smt_directory <> "" then
    begin
      let len = String.length !smt_directory in
      if !smt_directory.[len - 1] = '/' then
        smt_directory := String.sub !smt_directory 0 (len - 1);
      assert (Config.get_files () = []);
      let smtdirs = list_directories !smt_directory in
      List.iter
        (fun dir ->
         List.iter Config.add_file (list_smt2_files dir);
         execute_tests_on_files ~dir ();
         Config.clear_files ();
        )
        smtdirs;
    end
  else execute_tests_on_files ()
;;

main ()

(* Local Variables: *)
(* compile-command: "make tester" *)
(* End: *)
