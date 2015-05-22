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

(* Quick fix to remove bound but unused variables or unused function paramters *)
(* let unused_locals = Hashtbl.create 7 ;; *)

let all_defined (vs : varstate) =
  SymbolSet.union vs.user_defined vs.theory_defined
;;

let define vs sy =
  { vs with user_defined = SymbolSet.add sy vs.user_defined; }
;;

let use vs sy =
  { vs with used = SymbolSet.add sy vs.used; }
;;

let compute_uu (vs : varstate) =
  let unused = SymbolSet.diff vs.user_defined vs.used in
  let undefined =
    SymbolSet.diff vs.used (SymbolSet.union vs.user_defined vs.theory_defined)
  in unused, undefined
;;

exception Ellipsis ;;
let ppset fmt (s : SymbolSet.t) =
  let n = ref 0 in
  try
    SymbolSet.iter
      (fun s ->
       if !n > 10 then raise Ellipsis
       else (Format.fprintf fmt "%a;@ " Pp.pp_symbol s; incr n)) s
  with Ellipsis ->
    Format.fprintf fmt "... (%d more)@." (SymbolSet.cardinal s - !n)
;;

let pp_uu fmt (unused, undef) =
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

let pp_symbols fmt (sys : SymbolSet.t) =
  SymbolSet.iter
    (fun sy -> Format.fprintf fmt "%a;@ " Pp.pp_symbol sy) sys
;;

let svar_set (svars: Ast.sorted_vars) =
  List.fold_left
    (fun s sv -> SymbolSet.add (Ast_utils.symbol_of_svar sv) s)
    SymbolSet.empty svars
;;

let limit = ref 10 ;;
let handle_locally_unused (unused : SymbolSet.t) (loc: Locations.t) =
  let n = SymbolSet.cardinal unused in
  if n < !limit then
    Io.warning
      "@[<v 0>Unused bounded variables at %a @[<hov 0>%a@]@]@."
      Pp.pp_loc loc pp_symbols unused
  else Io.warning
         "@[<v 0>%d unused bounded variables at %a@]@."
         n Pp.pp_loc loc;
(*  SymbolSet.iter
    (fun s -> Hashtbl.add unused_locals s ())
    unused; *)
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
  | TermSpecConstant _ -> vs, t
  | TermQualIdentifier qid -> eval_qual_identifier vs qid, t
  | TermQualIdentifierTerms (qid, terms) ->
     let vs, terms = eval_terms vs terms in
     eval_qual_identifier vs qid,
     { t with term_desc = TermQualIdentifierTerms(qid, terms) }

  | TermLetTerm (vbindings, term) ->
     let vsbindings, vbindings  =
       List.fold_left
         (fun (vs, vbs) vb ->
          let vs, vb' = eval_var_binding vs vb in vs, vb' :: vbs)
         (empty_vstate, [])
         vbindings in
     let vs' = { vs with
                 user_defined =
                   SymbolSet.union vsbindings.user_defined vs.user_defined;
                 used = SymbolSet.empty; } in
     let vsterm, term' = eval_term vs' term in
     let outside_used = SymbolSet.diff vsterm.used (all_defined vsbindings) in
     let unused_bindings = SymbolSet.diff vsbindings.user_defined vsterm.used in
     let vbindings' =
       if not (SymbolSet.is_empty unused_bindings) then
         begin
           handle_locally_unused unused_bindings t.term_loc;
           if Config.get_rm_unused () then
             List.fold_left
               (fun l vb ->
                if SymbolSet.mem
                     (Ast_utils.symbol_of_vbinding vb) unused_bindings
                then l
                else vb :: l
               )
               [] vbindings
           else List.rev vbindings
         end
       else List.rev vbindings
     in
     { vs with used = SymbolSet.union
                        vs.used
                        (SymbolSet.union outside_used vsbindings.used); }
     , if vbindings' = [] then term'
       else { t with term_desc = TermLetTerm (vbindings', term') }
  | TermForallTerm (svars, term) ->
     let vs, svars', term' = handle_quantifier vs svars term in
     vs,
     if svars' = [] then term'
     else { t with term_desc = TermForallTerm (svars', term') }
  | TermExistsTerm (svars, term) ->
     let vs, svars', term' = handle_quantifier vs svars term in
     vs,
     if svars' = [] then term'
     else { t with term_desc = TermExistsTerm (svars', term') }
  | TermAnnotatedTerm (term, annot) ->
     let vs, term' = eval_term vs term in
     vs, { t with term_desc = TermAnnotatedTerm(term', annot) }

and eval_var_binding (vs : varstate) (vb : Ast.var_binding) =
  match vb.var_binding_desc with
  | VarBinding (sy, term) ->
     let vs, term' = eval_term vs term in
     define vs sy, { vb with var_binding_desc = VarBinding(sy, term') }

and eval_terms vs terms =
  let vs, terms =
    List.fold_left
      (fun (vs, terms) t ->
       let vs, t' = eval_term vs t in
       vs, t' :: terms)
      (vs, []) terms
  in vs, List.rev terms

and handle_quantifier vs svars term =
  let new_symbols = svar_set svars in
  let user_defined = SymbolSet.union new_symbols vs.user_defined in
  let vsterm, term' = eval_term { empty_vstate with user_defined; } term in
  let outside_used = SymbolSet.diff vsterm.used new_symbols in
  let unused_bindings = SymbolSet.diff new_symbols vsterm.used in
   let svars' =
     if not (SymbolSet.is_empty unused_bindings) then
       begin
         handle_locally_unused unused_bindings term.term_loc;
         List.filter
           (fun svar ->
            not (SymbolSet.mem (Ast_utils.symbol_of_svar svar) unused_bindings)
           ) svars
       end
     else svars
   in
   { vs with used = SymbolSet.union outside_used vs.used; },
   svars', term'

;;

let eval_fundef (vs : varstate) (f : Ast.fun_def) =
  match f.fun_def_desc with
  | FunDef (sy, optsorts, svars, sort, body) ->
     let new_symbols = svar_set svars in
     let fdefined = SymbolSet.union vs.user_defined new_symbols in
     let vs', _svars', body' =
       handle_quantifier
         { vs with user_defined = fdefined; used = SymbolSet.empty; }
         svars body
     in
     let outer_scope_used = SymbolSet.diff vs'.used new_symbols in
     let used' = SymbolSet.union vs.used outer_scope_used in
     define { vs with used = used' } sy,
     { f with fun_def_desc = FunDef(sy, optsorts, svars, sort, body') }
;;

let eval_command ((vs, cmds) : varstate * Ast.commands) (cmd : Ast.command) =
  match cmd.command_desc with
  | CmdAssert t ->
     let vs, t' = eval_term vs t in
     vs, { cmd with command_desc = CmdAssert t' } :: cmds
  | CmdDeclareConst (sy, _)
  | CmdDeclareFun (sy, _, _, _) -> define vs sy, cmd :: cmds
  | CmdDefineFun fdef ->
     let vs, fdef = eval_fundef vs fdef in
     vs, { cmd with command_desc = CmdDefineFun fdef } :: cmds
  | CmdCheckSatAssuming symbols ->
     List.fold_left use vs symbols, cmd :: cmds
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
  | CmdSetOption _ -> vs, cmd :: cmds
;;

let eval_commands vs cmds =
  let vs, cmds = List.fold_left eval_command (vs, []) cmds in
  vs, List.rev cmds
;;

let compute (script: Extended_ast.ext_script) =
  let theory_defined =
    List.fold_left
      (fun s (name, _) -> SymbolSet.add (Ast_utils.mk_symbol name) s)
      SymbolSet.empty script.ext_script_theory.theory_symbols
  in
  let vstate = { empty_vstate with theory_defined; } in
  let vs, ext_script_commands =
    eval_commands vstate script.ext_script_commands in
  let unused, undefined = compute_uu vs in
  unused, undefined, { script with ext_script_commands }
;;

let useful_command (unused_symbols : SymbolSet.t) cmd =
  match cmd.command_desc with
  | CmdDefineFun ({fun_def_desc = FunDef(sy, _, _, _, _); _ })
  | CmdDeclareFun (sy, _, _, _)
  | CmdDeclareConst (sy, _) -> not (SymbolSet.mem sy unused_symbols)
  | _ -> true
;;

let apply (script: Extended_ast.ext_script) =
  if Config.get_unused () then
    let unused, undefined, script = compute script in
    if Config.get_rm_unused () then
      { script with
        ext_script_commands =
          List.filter (useful_command unused) script.ext_script_commands;
      }
    else (Format.printf "%a@." pp_uu (unused, undefined); script)
  else script
;;
