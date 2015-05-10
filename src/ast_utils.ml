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

open Ast ;;

let symbol_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with
  | SortedVar (sy, _) -> sy
;;

let sort_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with
  | SortedVar (_, so) -> so
;;

let symbol_from_id (id : Ast.identifier) =
    match id.id_desc with
    | IdSymbol sy -> sy
    | IdUnderscore (sy, _) -> sy
;;

let symbols_of_sort (sort : Ast.sort) : Ast.symbol list =
  let rec aux symbols sort =
    match sort.sort_desc with
    | SortIdentifier id -> (symbol_from_id id) :: symbols
    | SortFun (id, sorts) ->
       List.fold_left aux ((symbol_from_id id) :: symbols) sorts
  in List.rev (aux [] sort)
;;

let string_of_symbol (symbol : Ast.symbol) : string =
  match symbol.symbol_desc with
  | SimpleSymbol s
  | QuotedSymbol s -> s
;;

let symbol_of_vbinding (vb : var_binding) =
  match vb.var_binding_desc with
  | VarBinding (sy, _) -> sy
;;

let id_from_qid (qid : Ast.qual_identifier) : identifier =
  match qid.qual_identifier_desc with
  | QualIdentifierAs (id, _)
  | QualIdentifierIdentifier id -> id
;;

let get_logic (s : Ast.script) =
  let rec aux (cmds : Ast.commands) =
    match cmds with
    | [] -> ""
    | { command_desc = CmdSetLogic symb; _ } :: _ ->
       begin
         match symb.symbol_desc with
           | SimpleSymbol logic_name -> logic_name
           | QuotedSymbol _ -> assert false
       end
    | _ :: cmds -> aux cmds
  in aux s.script_commands
;;

let rec is_constant_term (t : Ast.term) : bool =
  match t.term_desc with
  | TermSpecConstant _ -> true
  | TermAnnotatedTerm (t, _) -> is_constant_term t
  | TermLetTerm _
  | TermQualIdentifier _
  | TermQualIdentifierTerms _
  | TermForallTerm _
  | TermExistsTerm _ -> false
;;

let is_variable_term (t : Ast.term) : bool =
  not (is_constant_term t)
;;

let mk_symbol (s:string) =
  { symbol_desc = SimpleSymbol s;
    symbol_loc = Locations.dummy_loc;
  }
;;

let mk_localized_symbol (s : string) (symbol_loc : Locations.t) : Ast.symbol =
  { symbol_desc = SimpleSymbol s;
    symbol_loc;
  }
;;

let mk_command (cmd : Ast.command_desc) : Ast.command =
  { command_desc = cmd; command_loc = Locations.dummy_loc; }
;;
