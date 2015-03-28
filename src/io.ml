(**************************************************************************)
(*  Copyright (c) 2015 Richard Bonichon <richard.bonichon@gmail.com>      *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for any  *)
(*  purpose with or without fee is hereby granted, provided that the above  *)
(*  copyright notice and this permission notice appear in all copies.     *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  *)
(*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF      *)
(*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *)
(*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES  *)
(*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN  *)
(*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *)
(*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.        *)
(*                                                                        *)
(**************************************************************************)

open Format
open Locations
open Lexing
;;

let not_yet_implemented (msg:string) =
  failwith (sprintf "not yet implemented: %s@." msg)
;;

let newline fmt =
  pp_force_newline fmt ();
;;

type output = {
  name : string;
  mutable fmt : Format.formatter;
}


let default_out name = { name; fmt = Format.std_formatter; } ;;

let debug_output = default_out "debug"
and log_output = default_out "log"
and warning_output = default_out "warning"
and res_output = default_out "result"
and error_output = { name = "error"; fmt = Format.err_formatter; }
;;

let set_formatter output fmt = output.fmt <- fmt ;;

let ouputs = [ debug_output; log_output; warning_output; error_output; ] ;;

let is_warn_err s =
  s = warning_output.name ||
    s = error_output.name


let glog tag (output: output) txt  =
  let fmt = output.fmt in
  Format.fprintf fmt "@{<item>";
  (if tag then Format.fprintf fmt "[%s] " output.name
   else Format.ifprintf fmt "");

  Format.kfprintf
    (fun fmt ->
     Format.fprintf fmt "@}@?";
     Format.pp_print_flush fmt ();
    )
    fmt txt
;;

(** Various types of outputs *)
let set_tagging, get_tagging =
  let tag = ref true in
  (fun ta -> tag := ta),
  (fun () -> !tag)
;;

let debug txt =
   if Config.get_debug () then glog (get_tagging()) debug_output txt
   else Format.ifprintf Format.std_formatter txt
;;

let warning txt = glog (get_tagging ()) warning_output txt;;

let error txt =
  glog (get_tagging ()) error_output txt ;;

let result txt = glog false res_output txt;;

let log txt = glog (get_tagging ()) log_output txt ;;

let warn _loc msg =  warning "%s" msg ;;



module Error = struct
exception Lex_error of string;;

let char_num pos = pos.pos_cnum - pos.pos_bol ;;

let errpos pos msg =
  let s = Format.sprintf "File \"%s\", line %d, character %d:"
    pos.Lexing.pos_fname pos.Lexing.pos_lnum (char_num pos)
  in error "@[%s : %s@]@." s msg;
  exit 2;
;;

let errloc loc msg =
  let lstart = loc.loc_start and lend = loc.loc_end in
  let s = Format.sprintf
    "@[<hov 1>File \"%s\",@ from line %d character %d@ to line %d character %d@]"
    lstart.pos_fname lstart.pos_lnum
    (char_num lstart)
    lend.pos_lnum
    (char_num lend)
  in error "@[%s : %s@]@." s msg;
  exit 2;
;;
let report_error lbuf msg =
  let p = Lexing.lexeme_start_p lbuf in
  errpos p msg;
;;

end ;;

let fail loc msg = Error.errloc loc msg ;;
