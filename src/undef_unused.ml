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

(**
 ** Computation of  undefined and unused variables.
 ** If a name has multiple types, any occurrence of the name will mark all of
 ** its types as used (no type distinction is made).
 *)

open Ast
open Extended_ast
open Theory
open Utils
;;

type varstate = {
    used : SymbolSet.t ;
    user_defined : SymbolSet.t;
    theory_defined : SymbolSet.t;
  }
;;

let empty_vstate = {
    used = SymbolSet.empty;
    user_defined = SymbolSet.empty;
    theory_defined = SymbolSet.empty;
} ;;

let all_defined (vs : varstate) =
  SymbolSet.union vs.user_defined vs.theory_defined
;;

let define vs sy =
  { vs with user_defined = SymbolSet.add sy vs.user_defined; }
;;

let use vs sy =
  { vs with used = SymbolSet.add sy vs.used; }
;;

let pp_symbols fmt (sys : SymbolSet.t) =
  SymbolSet.iter
    (fun sy -> Format.fprintf fmt "%a;@ " Pp.pp_symbol sy) sys
;;

let svar_set (svars: Ast.sorted_vars) =
  List.fold_left
    (fun s sv -> SymbolSet.add (Ast_utils.symbol_of_svar sv) s)
    SymbolSet.empty svars
;;

let eval_index (vs : varstate) (idx : Ast.index) =
  match idx with
  | IdxNum _ -> vs
  | IdxSymbol sy -> use vs sy
;;

let eval_identifier (vs : varstate) (id : Ast.identifier) =
  match id.id_desc with
  | IdSymbol sy -> use vs sy
  | IdUnderscore (sy, idxs) -> use (List.fold_left eval_index vs idxs) sy
;;

let eval_qual_identifier (vs : varstate) (qid : Ast.qual_identifier) =
  match qid.qual_identifier_desc with
  | QualIdentifierAs (id, _)
  | QualIdentifierIdentifier id -> eval_identifier vs id
;;

let rec eval_term (vs: varstate) (t: Ast.term) =
  match t.term_desc with
  | TermSpecConstant _ -> vs
  | TermQualIdentifier qid -> eval_qual_identifier vs qid
  | TermQualIdentifierTerms (qid, terms) ->
     eval_qual_identifier (eval_terms vs terms) qid
  | TermLetTerm (vbindings, term) ->
     let vsbindings = List.fold_left eval_var_binding empty_vstate vbindings in
     let vs' = { vs with
                 user_defined =
                   SymbolSet.union vsbindings.user_defined vs.user_defined;
                 used = SymbolSet.empty; } in
     let vsterm = eval_term vs' term in
     let outside_used = SymbolSet.diff vsterm.used (all_defined vsbindings) in
     let unused_bindings = SymbolSet.diff vsbindings.user_defined vsterm.used in
     if unused_bindings <> SymbolSet.empty then
       Format.printf
         "@[<v 0>Unused bounded variables: @[<hov 0>%a@]@]@."
         pp_symbols unused_bindings;
     { vs with used = SymbolSet.union outside_used vsbindings.used; }
  | TermForallTerm (svars, term)
  | TermExistsTerm (svars, term) ->
     let new_symbols = svar_set svars in
     let user_defined = SymbolSet.union new_symbols vs.user_defined in
     let vsterm = eval_term { empty_vstate with user_defined; } term in
     let outside_used = SymbolSet.diff vsterm.used new_symbols in
     { vs with used = SymbolSet.union outside_used vs.used; }
  | TermAnnotatedTerm (term, _) -> eval_term vs term

and eval_var_binding (vs : varstate) (vb : Ast.var_binding) =
  match vb.var_binding_desc with
  | VarBinding (sy, term) -> define (eval_term vs term) sy

and eval_terms vs terms =
  List.fold_left eval_term vs terms
;;

let eval_fundef (vs : varstate) (f : Ast.fun_def) =
  match f.fun_def_desc with
  | FunDef (sy, _, svars, _, term) ->
     let new_symbols = svar_set svars in
     let fdefined = SymbolSet.union vs.user_defined new_symbols in
     let vs' =
       eval_term
         { vs with user_defined = fdefined; used = SymbolSet.empty; } term in
     let unused_params = SymbolSet.diff fdefined vs'.used in
     if unused_params <> SymbolSet.empty then
       Format.printf
         "@[<v 0>Unused parameters for function %a:@ @[<hov 0>%a@]@]@."
         Pp.pp_symbol sy
         pp_symbols unused_params;
     let outer_scope_used = SymbolSet.diff vs'.used fdefined in
     let used' = SymbolSet.union vs.used outer_scope_used in
     define { vs with used = used' } sy
;;

let eval_command (vs : varstate) (cmd : Ast.command) =
  match cmd.command_desc with
  | CmdAssert t -> eval_term vs t
  | CmdDeclareConst (sy, _)
  | CmdDeclareFun (sy, _, _, _) -> define vs sy
  | CmdDefineFun fdef -> eval_fundef vs fdef
  | CmdCheckSatAssuming symbols -> List.fold_left use vs symbols
  | CmdDefineFunRec _fdef_rec -> Io.not_yet_implemented "Recursive function"
  | CmdCheckSat
  | CmdDeclareSort _
  | CmdDefineSort _
  | CmdEcho _
  | CmdExit
  | CmdGetAssertions
  | CmdGetAssignment
  | CmdGetInfo _
  | CmdGetModel
  | CmdGetOption _
  | CmdGetProof
  | CmdGetUnsatAssumptions
  | CmdGetUnsatCore
  | CmdGetValue _
  | CmdMetaInfo _
  | CmdPop _
  | CmdPush _
  | CmdReset
  | CmdResetAssertions
  | CmdSetInfo _
  | CmdSetLogic _
  | CmdSetOption _ -> vs
;;

let eval_commands vs cmds =
  List.fold_left eval_command vs cmds
;;

let apply (script: Extended_ast.script) =
  let theory_defined =
    List.fold_left
      (fun s (name, _) -> SymbolSet.add (Ast_utils.mk_symbol name) s)
      SymbolSet.empty script.script_theory.theory_symbols
  in
  let vstate = { empty_vstate with theory_defined; } in
  let vs = eval_commands vstate script.script_commands in
  let unused = SymbolSet.diff vs.user_defined vs.used in
  let undefined =
    SymbolSet.diff vs.used (SymbolSet.union vs.user_defined vs.theory_defined)
  in unused, undefined
;;


let ppset fmt (s : SymbolSet.t) =
  SymbolSet.iter (fun s -> Format.fprintf fmt "%a;@ " Pp.pp_symbol s) s
;;

let pp_result fmt (unused,undef) =
  Format.fprintf fmt "@[<v 0>";
  if not (SymbolSet.is_empty unused) then
    begin
      Format.fprintf fmt
        "@[<v 2>%a@ %a@]@ "
        Utils.mk_header "Unused symbols"
        ppset unused
    end;
  if not (SymbolSet.is_empty undef) then
    begin
      Format.fprintf fmt
        "@[<v 2>%a@ %a@]"
        Utils.mk_header "Undef symbols"
        ppset undef
    end;
  Format.fprintf fmt "@]";
;;

let apply_and_pp (script : Extended_ast.script) =
  Format.printf "%a@." pp_result (apply script)
;;
