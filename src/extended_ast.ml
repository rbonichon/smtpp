(**************************************************************************)
(*  Copyright (c) 2015, INRIA, Universite de Nancy 2 and Universidade     *)
(*  Federal do Rio Grande do Norte.                                       *)
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
(**************************************************************************)

open Logic ;;

type ext_script = {
    ext_script_commands : Ast.commands ;
    ext_script_loc : Locations.t ;
    ext_script_theory : Theory.t ;
  }
;;

let to_ast_script (s : ext_script) =
  { Ast.script_commands = s.ext_script_commands;
    Ast.script_loc = s.ext_script_loc;
  }
;;

let load_theories (s : Ast.script) : ext_script =
  let theory = ref Theory.SMTCore.theory in
  let add_theory (th : Theory.t) =
    theory := Theory.combine !theory th;
  in
  let logic_name = Ast_utils.get_logic s in
  let t = parse_logic logic_name in
  if t.array then add_theory Theory.SMTArray.theory;
  if t.bitvectors then add_theory Theory.SMTBitVectors.theory;
  (match t.arithmetic_sort with
   | Some Integer -> add_theory Theory.SMTInt.theory
   | Some Real -> add_theory Theory.SMTReal.theory
   | Some Mixed -> add_theory Theory.SMTNumerics.theory
   | None -> ()
  );
  { ext_script_commands = s.Ast.script_commands;
    ext_script_loc = s.Ast.script_loc;
    ext_script_theory = !theory;
  }
;;

let set_logic (logic : Logic.t) (ext_script : ext_script) : ext_script =
  let open Ast in
  let logic_name = Utils.sfprintf "%a" Logic.pp_from_core logic in
  match ext_script.ext_script_commands with
  | [] -> ext_script
  | { command_desc = CmdSetLogic sy; command_loc; } :: cmds ->
     let logic_symbol =
       Ast_utils.mk_localized_symbol logic_name sy.symbol_loc in
     { ext_script
     with ext_script_commands =
            { command_desc = CmdSetLogic logic_symbol; command_loc }
            :: cmds }
  | cmds ->
     let logic_symbol =
       Ast_utils.mk_localized_symbol logic_name Locations.dummy_loc in
      { ext_script
      with ext_script_commands =
             { command_desc = CmdSetLogic logic_symbol;
               command_loc = Locations.dummy_loc }
            :: cmds }
;;
