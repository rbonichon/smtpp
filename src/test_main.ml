(*********************************************************************************)
(*  Copyright (c) 2015, INRIA, Universite de Nancy 2 and Universidade Federal    *)
(*  do Rio Grande do Norte.                                                      *)
(*                                                                               *)
(*  Permission to use, copy, modify, and distribute this software for any        *)
(*  purpose with or without fee is hereby granted, provided that the above       *)
(*  copyright notice and this permission notice appear in all copies.            *)
(*                                                                               *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES     *)
(*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF             *)
(*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR      *)
(*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES       *)
(*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN        *)
(*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF      *)
(*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.               *)
(*********************************************************************************)

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

let summary_fmt = ref Format.std_formatter ;;

let chop_path_prefix path1 path2 =
  let rec aux basenames dirname =
    if String.compare dirname path1 = 0
       || String.compare dirname "." = 0
    then
      begin
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

let link_to link fmt s =
  Format.fprintf fmt "More details at [%s](%s)." s link
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
  let header = Filename.basename dir in
  pre_tests header;
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
           Io.log "ERROR %s@." !current_file;
           incr errors;)
     end;
     close ();
    ) (Config.get_files ());
  Format.fprintf !fmt "END@ ~~~@]@.@.";
  post_tests header;
  Format.fprintf !fmt "* **Errors** : %d@.@." !errors;
  Format.pp_print_flush !fmt ();
  close_log ()
;;

let init_test_detection, test_detection , end_test_detection =
  let base_link = "td" in
  let h = Hashtbl.create 7 in
  let ntests = ref 0 in
  let alerts = ref 0 in
  let over = ref 0 in
  let under = ref 0 in
  let both = ref 0 in
  let link = ref "" in
  (fun header ->
   Hashtbl.reset h;
   ntests := 0;
   over := 0;
   under := 0;
   both := 0;
   alerts := 0;
   link := Format.sprintf "#%s%s" base_link header;
   Format.fprintf !summary_fmt "## %s@." header;
   Format.fprintf !fmt "## %s : logic {%s}@." header !link;
   Format.fprintf !fmt "### Raw results@.@."
  ),
  (fun s ->
   incr ntests;
   let s = Extended_ast.to_ast_script s in
   let detected_logic = Inferlogic.detect_logic s in
   let declared_logic = Logic.parse_logic (Ast_utils.get_logic s) in
   if not (Logic.equal detected_logic declared_logic) then
     begin
       incr alerts;
       Format.fprintf
         !fmt
         "@[<v 4>%d. %s@ %a (declared), %a (detected)@]@ "
         !alerts
         (chop_path_prefix !smt_directory !current_file)
         Logic.pp_from_core declared_logic
         Logic.pp_from_core detected_logic;
       (try
         let v = Hashtbl.find h detected_logic in
         Hashtbl.replace h detected_logic (succ v)
         with Not_found -> Hashtbl.add h detected_logic 1);
       (match Logic.one_bigger_dimension detected_logic declared_logic,
              Logic.one_bigger_dimension declared_logic detected_logic
        with
        | true, true -> incr both
        | true, false -> incr over
        | false, true -> incr under
        | false, false -> assert false);
     end
  ),
  (fun header ->
   Format.fprintf
     !summary_fmt
     "@[<v 0>\
      ### %s : summary of logic detection@ \
      * Alerts : %d / %d@ \
      * Over : %d@ \
      * Under : %d@ \
      * OverUnder : %d@ \
      * @[<v 4>**By categories** :@ %a@] \
      @ \
      %a\
      @]@.@.\
      "
     header
     !alerts !ntests !over !under !both
     (fun fmt h ->
      Hashtbl.iter
        (fun k v -> Format.fprintf fmt "* %a : %d@ " Logic.pp_from_core k v)
        h
     ) h
     (link_to !link) header
   ;
  )
;;

let grep_count name file =
  (* When greping a name in a file, we must take care in not counting
   * neq_unit when looking for eq_unit or equals when looking for equal.
   * In SMT-LIB, this means there is at least one space or a parenthesis before
   * and a space or a parenthesis after the name.
   *)
  let command = Format.sprintf "grep -c \"[( ]%s[ )]\" %s" name file in
  let ic = Unix.open_process_in command in
  let n = int_of_string (input_line ic) in
  ignore(Unix.close_process_in ic);
  n
;;

let init_test_use_def, test_use_def, end_test_use_def =
  let base_link = "uu" in
  let alerts = ref 0 in
  let ntests = ref 0 in
  let nunused = ref 0 in
  let with_errors = ref 0 in
  let nundef = ref 0 in
  let link = ref base_link in
  (fun header ->
   ntests := 0;
    alerts := 0;
   nundef := 0;
   nunused := 0;
   with_errors := 0;
   link := Format.sprintf "#%s%s" base_link header;
   Format.fprintf !fmt "## %s : undef/unused {#%s}@." header !link;
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
       !fmt "%d. @[<v 4>%s@ @ %a@ "
       !alerts
       (chop_path_prefix !smt_directory !current_file)
       Undef_unused.pp_uu uures
     end;
   if has_unused then begin
       let card = SymbolSet.cardinal unused in
       let ncheck = ref 0 in
       let check_unused =
         SymbolSet.fold
           (fun symb l ->
            incr ncheck;
            if !ncheck < 500 then 
                let vname = Ast_utils.string_of_symbol symb in
                Io.log "Checking %d/%d : %s@." !ncheck card !current_file;
                let n = grep_count vname !current_file in
                if n > 1 then vname :: l
                else l
            else l
           ) unused []
       in
       if check_unused <> [] then
         begin
           Format.fprintf !fmt "@[<hov 4>**Double-check unused**@ ";
           List.iter (fun name -> Format.fprintf !fmt "%s;@ " name) check_unused;
           Format.fprintf !fmt "@]@.";
           incr with_errors;
         end;
     end;
   Format.fprintf !fmt "@]@.";
   if has_unused then incr nunused ;
   if has_undef then incr nundef;
  ),
  (fun header ->
   Format.fprintf
     !summary_fmt
     "@[<v 0>\
      ### %s : summary of uu detection@ \
      * Alerts : %d / %d@ \
      * With unused : %d@ \
      * With undefined : %d@ \
      * With double-checks: %d@ \
      %a@ \
      @]@.\
      "
     header
     !alerts !ntests !nunused !nundef !with_errors 
     (link_to !link) header
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
  let sbname = "prelude.md" in
  let fname_summary =
    if !output_directory = "" then sbname
    else Filename.concat !output_directory sbname in
  let sum_oc = open_out fname_summary in
  summary_fmt := Format.formatter_of_out_channel sum_oc;
  Format.fprintf !summary_fmt "# Summary of results@.";
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
  else execute_tests_on_files ();
    Format.fprintf !summary_fmt "# Detailed results@.";
  Format.pp_print_flush !summary_fmt ();
  close_out sum_oc;
;;

main ()

(* Local Variables: *)
(* compile-command: "make tester" *)
(* End: *)
