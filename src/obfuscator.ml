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

open Ast ;;
open Format ;;

module H = Hashtbl ;;

exception Found of symbol ;;

let pick_one h =
  try Hashtbl.iter (fun _ v -> raise (Found v)) h;
      assert false; (* Should not get here *)
  with Found v -> v
;;

let add_sorted_symbol, _find_sorted_symbol, find_symbol =
  let h = Hashtbl.create 97 in
  let n = ref (-1) in
  let base = "S" in
  (fun symbol sort ->
   incr n;
   let newsymb =
     { symbol with symbol_desc = SimpleSymbol (base ^ (string_of_int !n)) }
   in
   (try
       let hsort = Hashtbl.find h symbol in
       Hashtbl.add hsort sort newsymb
     with
     | Not_found ->
        begin
          let hsort = Hashtbl.create 7 in
          Hashtbl.add hsort sort newsymb;
          Hashtbl.add h symbol hsort;
        end;);
     newsymb
  ),
  (fun symbol sort -> H.find (H.find h symbol) sort),
  (fun symbol -> let hsort = H.find h symbol in pick_one hsort)
;;

let obfuscate_command cmd =
  let command_desc =
    match cmd.command_desc with
    | CmdReset
    | CmdCheckSat
    | CmdEcho _
    | CmdExit
    | CmdGetAssertions
    | CmdGetModel
    | CmdGetAssignment
    | CmdGetProof
    | CmdGetUnsatCore
    | CmdGetUnsatAssumptions
    | CmdPop _
    | CmdPush _
    | CmdGetOption _
    | CmdGetInfo _
    | CmdMetaInfo _ as c -> c
    | CmdDeclareConst (symb, sort)  ->
       let s = add_sorted_symbol symb sort in
       CmdDeclareConst (s, sort)
    | CmdCheckSatAssuming symbs ->
       CmdCheckSatAssuming (List.map find_symbol symbs)
  in { cmd with command_desc }
;;

let obfuscate_commands cmds = List.map obfuscate_command cmds ;;

let apply script =
  let script_commands = obfuscate_commands script.script_commands in
  let obfuscated_script = { script with script_commands } in
  printf "%a" Pp.pp obfuscated_script
;;
