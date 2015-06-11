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

open Lexing;;

let report_error l  =
  let pos = lexeme_start_p l in
  let o = pos.pos_cnum - pos.pos_bol in
  Format.eprintf
    "Error in file %s, line %d, column %d %s@."
    pos.pos_fname pos.pos_lnum o
    l.lex_buffer
  ;
;;

let lex_file fname =
  try
    let chan =
      match fname with
      | "-" -> stdin
      | file -> open_in file
    in
    let lexbuf = Lexing.from_channel chan in
    lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = fname;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
    (lexbuf, fun () -> close_in chan)
  with
    | Not_found -> exit 2;
;;

let (|>) f g = fun x -> f (g x) ;;
let apply () =
  let (lexbuf, close) = lex_file (List.hd (Config.get_files ()))  in
  try
    let script = Parser.script Lexer.token lexbuf in
    let ext_script =
      (Satinfo.add_sat_status |> Undef_unused.apply) (Extended_ast.load_theories script) in
    Io.debug "Parsing and elaboration done@.";
    (*if Config.get_unused () then Undef_unused.apply_and_pp ext_script; *)

    let ext_script =
      if Config.get_detect () then
        Extended_ast.set_logic
          (Inferlogic.detect_and_print script)
          ext_script
      else ext_script
    in
    if Config.get_pushpop () then Pushpop.apply script;
    if Config.get_obfuscate () then Obfuscator.apply ext_script;
    if Config.get_reprint () then
      Pp.pp Format.std_formatter (Extended_ast.to_ast_script ext_script) ;
    if Config.get_preLA () then Pre_LA.pre_LA Format.std_formatter script; 
    if Config.get_preNLA () then Pre_LA.pre_NLA Format.std_formatter script;   
(*    if Config.get_preprocessor () then Preprocessor.apply script; *)
    close ();
  with
  | Lexer.LexError msg ->
     Format.eprintf "Lex error: %s@." msg;
     report_error lexbuf
  | Parser.Error  ->
     Format.eprintf "Parse error:@.";
     report_error lexbuf
;;
