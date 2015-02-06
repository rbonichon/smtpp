open Format ;;
let exe = ref "smtpp.opt"
and opts = ref ""
;;

let files = ref [] ;;
let failed_files = ref [] ;;
let add_file f = files := f :: !files ;;

(* Are these two files different ? *)
let diff fname1 fname2 =
  let cmd = sprintf "diff %s %s >/dev/null" fname1 fname2 in
  Sys.command cmd = 1
;;

let test_pp, get_pp_stats  =
  let errors = ref 0
  and failed = ref [] in
  let add_failed fname = (incr errors; failed := fname :: !failed) in
  (fun f ->
  let new_file () = Filename.temp_file "smt_pp" ".ast" in
  let pp_fname1 = new_file ()
  and pp_fname2 = new_file () in
  printf "Parsing %s@." f;
  let write_cmd = sprintf "cat %s | %s -pp > %s" f !exe pp_fname1 in
  printf "Parsing %s@." pp_fname1;
  let reparse_and_write = 
    sprintf "cat %s | %s -pp > %s" pp_fname1 !exe pp_fname2 in
  let n1 = Sys.command write_cmd in
  let n2 = Sys.command reparse_and_write in
  if n1 <> 0 || n2 <> 0 then failwith (sprintf "Failed pp test with %s\n" f);
  if diff pp_fname2 pp_fname1 then add_failed f
  else (
    Sys.remove pp_fname1;
    Sys.remove pp_fname2;
  )),
  (fun () -> !errors, !failed )
  
;;


(* @runs the command [cmd].
   @returns the value and the time taken.
 *)
let time_command cmd =
  let init = Sys.time () in
  let ret = Sys.command cmd in
  ret, (Sys.time () -. init)
;;

let fres, ores, ofmt   =
  let fres, ores = Filename.open_temp_file "smtparsetest" ".log" in
  let fmt = Format.formatter_of_out_channel ores in
  fres, ores, fmt
;;

let max_time = ref min_float
and min_time = ref max_float
and errors = ref 0
;;

let time_parser f =
  printf "parsing %s@." f;
  let cmd = sprintf "%s %s %s" !exe !opts f in
  let vret, t = time_command cmd in
  if vret <> 0 then (incr errors; failed_files := f :: !failed_files;);
  if t < !min_time then min_time := t;
  if t > !max_time then max_time := t;
  (* fprintf ofmt "%s, %d, %.5f@." f vret t; *)
;;


let umsg = "test_parser <file>+" ;;
let rec argspec = [
    "-exe", Arg.Set_string exe,
    " set smtpp.opt executable";
  ]

and usage () = Arg.usage (Arg.align argspec) umsg ;;

let main () =
  Arg.parse argspec add_file umsg;
  let n = List.length !files in
  let start = Sys.time () in
  (* rintf "outputting results in %s@." fres;*)
  (* fprintf ofmt "filename, return value, time@.";*)
  List.iter time_parser !files;
  printf "parser %d files in %.4f (%d errors)@."
         n (Sys.time () -. start) !errors;
  printf "max time = %.4f; min_time = %.4f@." !max_time !min_time;
  List.iter (fun fname -> printf "Parsing failed for %s@." fname) !failed_files;
  failed_files := [];
  List.iter test_pp !files;
  let errs, failed = get_pp_stats () in
  printf "%d errors@." errs;
  List.iter (fun s -> printf "%s@." s) failed;
(*close_out ores;*)
;;

 main ()

(* Local Variables: *)
(* compile-command: "ocamlopt -o tester %f" *)
(* End: *)
