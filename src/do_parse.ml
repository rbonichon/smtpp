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

open Lexing;;

let report_error l  =
  let pos = lexeme_start_p l in
  let o = pos.pos_cnum - pos.pos_bol in
  Format.eprintf "Error in file %s, line %d, column %d@."
                 pos.pos_fname pos.pos_lnum o;
;;

let lex_file () =
  try
    let fname = Config.get_file () in
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

let apply () =
  let (lexbuf, _close) = lex_file () in
  try
    let script = Parser.script Lexer.token lexbuf in
    let ext_script = Extended_ast.load_theories script in
    if Config.get_unused () then Undef_unused.apply ext_script;
    if Config.get_detect () then Inferlogic.detect_logic script;
    if Config.get_pushpop () then Pushpop.apply script;
    if Config.get_reprint () then Pp.pp Format.std_formatter script;
(*    if Config.get_preprocessor () then Preprocessor.apply script; *)
    if Config.get_obfuscate () then Obfuscator.apply ext_script;
  with
  | Lexer.LexError msg ->
     Format.eprintf "Parse error: %s@." msg;
     report_error lexbuf
  | Parser.Error  ->
     Format.eprintf "Parse error:@.";
     report_error lexbuf
;;
